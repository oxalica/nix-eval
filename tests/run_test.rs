use nix_eval::expr::{eval::Evaluator, lower::lower};
use std::{env, fs, path::PathBuf};

fn run_test(base_dir: &str, mut f: impl FnMut(String) -> String) {
    let trust_me = env::var("TRUST_ME").is_ok();

    let mut tests: Vec<PathBuf> = fs::read_dir(base_dir)
        .unwrap()
        .map(|ent| ent.unwrap().path())
        .filter(|path| path.extension().map_or(false, |ext| ext == "nix"))
        .collect();
    tests.sort();

    let mut failed = Vec::new();
    for input_path in &tests {
        let test_name = input_path.file_stem().unwrap().to_string_lossy();
        println!("Testing {}", test_name);

        let ans_path = input_path.with_extension("out");
        let failed_path = input_path.with_extension("failed");

        let content = fs::read_to_string(&input_path).unwrap();
        let ret = f(content);
        if trust_me {
            fs::write(&ans_path, &ret).unwrap();
            let _ = fs::remove_file(&failed_path);
        } else {
            let answer = fs::read_to_string(&ans_path).unwrap();
            if ret == answer {
                let _ = fs::remove_file(&failed_path);
            } else {
                println!("Failed");
                fs::write(&failed_path, ret).unwrap();
                failed.push(test_name);
            }
        }
    }
    if !failed.is_empty() {
        panic!("{} tests failed: {:?}", failed.len(), failed);
    }
}

#[test]
fn lower_test() {
    run_test("tests/lower_test", |input| format!("{:#?}", lower(&input)))
}

#[test]
fn eval_test() {
    run_test("tests/eval_test", |input| {
        let e = lower(&input).unwrap();
        let eval = Evaluator::new();
        format!("{:#?}", eval.eval_expr(&e, true))
    })
}
