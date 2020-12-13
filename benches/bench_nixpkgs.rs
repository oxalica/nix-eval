use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nix_eval::expr::lower::lower;
use once_cell::sync::Lazy;
use std::{fs, path::PathBuf, process::Command};

const NIXPKGS_REV: &str = "e9158eca70ae59e73fae23be5d13d3fa0cfc78b4";
const NIXPKGS_SHA256: &str = "0cnmvnvin9ixzl98fmlm3g17l6w95gifqfb3rfxs55c0wj2ddy53";

static NIXPKGS_PATH: Lazy<PathBuf> = Lazy::new(|| {
    let expr = format!(
        r#"fetchTarball {{ url = "https://github.com/NixOS/nixpkgs/archive/{}.zip"; sha256 = "{}"; }}"#,
        NIXPKGS_REV, NIXPKGS_SHA256,
    );
    let output = Command::new("nix-instantiate")
        .args(&["--eval", "-E", &expr])
        .output()
        .expect("Failed to run nix-instantiate");
    if !output.status.success() {
        println!(
            "nix-instantiate returns {}: {}",
            output.status,
            String::from_utf8(output.stderr).unwrap(),
        );
    }
    let path = String::from_utf8(output.stdout).unwrap();
    let path = path.trim().trim_matches('"');
    println!("nixpkgs: {}", path);
    PathBuf::from(path.to_owned())
});

fn bench_lower(c: &mut Criterion) {
    let mut bench_file = |name, path| {
        let input = fs::read_to_string(NIXPKGS_PATH.join(path)).unwrap();
        c.bench_function(&format!("lower {} ({} bytes)", name, input.len()), |b| {
            b.iter_with_large_drop(|| lower(black_box(&input)).unwrap())
        });
    };

    bench_file("hello.nix", "pkgs/applications/misc/hello/default.nix");
    bench_file("all_packages.nix", "pkgs/top-level/all-packages.nix");
    bench_file(
        "node-packages.nix",
        "pkgs/development/node-packages/node-packages.nix",
    );
}

criterion_group!(benches, bench_lower);
criterion_main!(benches);
