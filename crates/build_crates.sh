#!/bin/bash
mkdir -p ./priv
cargo build --manifest-path=crates/word_sensitive/Cargo.toml --release
sh -c "cp $(cat crates/word_sensitive/libpath) ./priv/libword_sensitive.so "