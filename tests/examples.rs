use snake::runner::compile_and_run_file;

macro_rules! mk_test {
    ($test_name:ident, $file_name:expr, $expected_output:expr) => { 
	#[test]
	fn $test_name() -> std::io::Result<()> {
	    test_example_file($file_name, $expected_output)
	}
    }
}

macro_rules! mk_fail_test {
    ($test_name:ident, $file_name:expr, $expected_output:expr) => { 
	#[test]
	fn $test_name() -> std::io::Result<()> {
	    test_example_fail($file_name, $expected_output)
	}
    }
}


/* 
 * YOUR TESTS GO HERE
 */

/* If I want to name a test "test1" that compiles and runs the file
 * examples/foo.adder and I expect it to return 3 I would uncomment
 * the following line
*/
mk_test!(test1, "add1.adder", 5);
mk_test!(test2, "nestedAdd.adder", 6);
mk_test!(test3, "subtract.adder", 3);
mk_test!(test4, "let.adder", 3);
mk_test!(test5, "let1.adder", 3);

/*
 * If I want to name a test "fail_test1" that compiles and runs the
 * file examples/fail.adder and I expect it to raise an error message
 * containing the string "uh oh", I would uncomment the following line:
 */ 
 mk_fail_test!(fail_test1, "parse_error.adder", "uh oh"); 
 mk_fail_test!(fail_test2, "parse_error1.adder", "uh oh"); 


// IMPLEMENTATION
fn test_example_file(f: &str, expected: i64) -> std::io::Result<()> {
    use std::path::Path;
    let expected_str = format!("{}", expected);
    let tmp_dir = tempfile::TempDir::new()?;
    match compile_and_run_file(&Path::new(&format!("examples/{}", f)), tmp_dir.path()) {
	Ok(stdout) => { assert_eq!(stdout.trim(), expected_str) }
	Err(e) => { assert!(false, "Expected {}, got an error: {}", expected_str, e)}
    }
    Ok(())
}

fn test_example_fail(f: &str, includes: &str) -> std::io::Result<()> {
    use std::path::Path;
    let tmp_dir = tempfile::TempDir::new()?;
    match compile_and_run_file(&Path::new(&format!("examples/{}", f)), tmp_dir.path()) {
	Ok(stdout) => { assert!(false, "Expected a failure but got: {}", stdout.trim()) }
	Err(e) => {
	    let msg = format!("{}", e);
	    assert!(msg.contains(includes), "Expected error message to include the string \"{}\" but got the error: {}", includes, msg)}
    }
    Ok(())
}

