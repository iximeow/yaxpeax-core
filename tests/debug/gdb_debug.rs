use yaxpeax_core::debug::gdb_remote::GDBRemote;

#[ignore]
#[test]
fn test_gdb_read_mem() {
    let mut remote = GDBRemote::connect(&"127.0.0.1:1234".parse().unwrap()).unwrap();
    remote.set_thread(0).unwrap();
    println!("Connected...");
    remote.get_regs();
    assert_eq!(
        remote.read_memory(0x7ffff7dd7c00, 0x10).unwrap(),
        vec![0x00, 0x01, 0x02, 0x03]
    );
}
