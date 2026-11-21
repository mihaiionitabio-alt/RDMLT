"""Build the Fortran kernel natively and as an offline, embedded WebAssembly module.

Linux/WSL: python3 scripts/build_fortran.py --native --wasm
Requires gfortran, LFortran (LLVM backend), and Emscripten. No compiler is needed by users.
"""
from pathlib import Path
import argparse, hashlib, json, os, re, subprocess

ROOT = Path(__file__).resolve().parent
SRC = ROOT / "fortran"
BUILD = ROOT / "build"
ORDER = ["math", "chemistry", "thermal", "monitor", "analysis", "optics", "oligo", "browser", "font", "framebuffer"]

def run(args):
    subprocess.run([str(x) for x in args], cwd=BUILD, check=True)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--native", action="store_true")
    parser.add_argument("--wasm", action="store_true")
    options = parser.parse_args()
    BUILD.mkdir(parents=True, exist_ok=True)
    files = [SRC / ("r2_" + name + ".f90") for name in ORDER] + [SRC / (name + ".f90") for name in ["json_fortran_browser_strings","stdlib_browser_stats","native_scenarios","native_model","native_io","native_ui"]]
    source = "\n".join(p.read_text(encoding="utf-8") for p in files)
    if options.native:
        native_mods=BUILD/"gnu-modules"
        native_mods.mkdir(exist_ok=True)
        subprocess.run([str(x) for x in [os.environ.get("FC", "gfortran"), "-std=f2018", "-ffree-line-length-none", "-O2", "-fno-fast-math", "-ffp-contract=off",
             "-fcheck=all", "-fPIC", "-shared", *files, "-o", BUILD / "libr2.so"]],cwd=native_mods,check=True)
    if options.wasm:
        compiler = os.environ.get("LFORTRAN", "lfortran")
        (BUILD / "kernel.f90").write_bytes(source.encode("utf-8"))
        run([compiler, "--fast", "--no-fast-math", "--target=wasm32-unknown-emscripten", "--separate-compilation", "--rtlib",
             "-c", BUILD / "kernel.f90", "-o", BUILD / "r2.o"])
        exports = sorted(set(re.findall(r"(?:function|subroutine)\s+(native_\w+)\([^\n]*\)[^\n]*bind\(c\)", source, re.I)))
        prefix = Path(compiler).resolve().parents[1] if "/" in compiler else None
        runtime = os.environ.get("LFORTRAN_WASM_RUNTIME")
        if not runtime and prefix:
            runtime = str(prefix / "share/lfortran/lib/lfortran_runtime_wasm_emcc.o")
        if not runtime or not Path(runtime).is_file():
            raise RuntimeError("Set LFORTRAN_WASM_RUNTIME to lfortran_runtime_wasm_emcc.o")
        target = ROOT / "native-engine.js"
        run(["emcc", BUILD / "r2.o", runtime, "-O0", "--no-entry", "-sSINGLE_FILE=1", "-sMODULARIZE=1",
             "-sEXPORT_NAME=R2FortranModule", "-sWASM_ASYNC_COMPILATION=0", "-sALLOW_MEMORY_GROWTH=1",
             "-sENVIRONMENT=web,node", "-sSTACK_SIZE=8388608", "-sINITIAL_MEMORY=67108864",
             "-sEXPORTED_FUNCTIONS=" + json.dumps(["_" + n for n in exports] + ["_malloc", "_free"]),
             "-o", target])
        info = {"schema": "missy-r2-fortran-build/1", "sourceSha256": hashlib.sha256(source.encode()).hexdigest(),
                "compiler": subprocess.check_output([compiler, "--version"], text=True).strip(),
                "linker": subprocess.check_output(["emcc", "--version"], text=True).splitlines()[0],
                "exports": exports, "embeddedWasm": True, "optimization": "LFortran --fast --no-fast-math; Emscripten -O0 link",
                "generatedSha256": hashlib.sha256(target.read_bytes()).hexdigest()}
        (ROOT / "build-info.json").write_bytes((json.dumps(info, indent=2) + "\n").encode())
        (ROOT / "build-info.js").write_bytes(("window.R2_FORTRAN_BUILD_INFO=" + json.dumps(info) + ";\n").encode())
        print(json.dumps({"exports": len(exports), "embeddedBytes": target.stat().st_size}))

if __name__ == "__main__":
    main()
