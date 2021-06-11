#!/bin/bash
mkdir -p ./priv
update(){
    cargo update --manifest-path=crates/word_sensitive/Cargo.toml 
}
build(){
    mkdir -p ./priv
    cargo build --manifest-path=crates/word_sensitive/Cargo.toml --release
    sh -c "cp $(cat crates/word_sensitive/libpath) ./priv/libword_sensitive.so "
}
test(){
    cargo test --manifest-path=crates/word_sensitive/Cargo.toml 
}
clippy(){
    cargo clippy --manifest-path=crates/word_sensitive/Cargo.toml 
}
help(){
    echo "sh build_crates.sh <command> :"
    echo "build              - do cargo build and cp libpath to priv"
    echo "test               - do cargo test"
    echo "clippy             - do cargo clippy to improve your rust code"
    echo "bench              - do cargo bench"
    echo "help               - help to use command"
}
bench(){
    cargo bench --manifest-path=crates/word_sensitive/Cargo.toml 
}
fmt(){
    cargo fmt --manifest-path=crates/word_sensitive/Cargo.toml 
}
case $1 in
fmt) fmt;;
bench) bench;;
build) build;;
test) test;;
clippy) clippy;;
update) update;;
*) help;;
esac