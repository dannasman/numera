use numera::program;

#[test]
fn test_program() {
    let source = &b"define int main() {\n\treturn 0;\n}"[..];
    assert_eq!(Ok(()), program(source));
}
