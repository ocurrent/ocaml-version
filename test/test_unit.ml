let test_configure_options =
  let test name version option ~expected =
    ( name,
      `Quick,
      fun () ->
        let got =
          Ocaml_version.Configure_options.to_configure_flag version option
        in
        Alcotest.check Alcotest.string __LOC__ expected got )
  in
  let v3_12 = Ocaml_version.of_string_exn "3.12.1" in
  [
    test "FP on last 4.07" Ocaml_version.Releases.v4_07 `Frame_pointer
      ~expected:"-with-frame-pointer";
    test "FP on first 4.08" Ocaml_version.Releases.v4_08_0 `Frame_pointer
      ~expected:"--enable-frame-pointers";
    test "FP on 3.12" v3_12 `Frame_pointer ~expected:"-with-frame-pointer";
  ]

let test_compiler_variants =
  let test ?(expect_exists=true) name arch version ~expected =
    ( name,
      `Quick,
      fun () ->
        let got = Ocaml_version.compiler_variants arch version in
        let all_ok = expected |> List.for_all (
          fun expected_extra ->
            let exists = got |> List.exists (fun v -> Ocaml_version.extra v = Some expected_extra) in
            exists = expect_exists
        ) in
        Alcotest.(check bool __LOC__ all_ok true) )
  in
  Ocaml_version.Releases.([
    test "Multicore not on 4.12 x86-64" `X86_64 v4_12
      ~expect_exists:false ~expected:["domains"; "domains+effects"];
    test "Multicore not on 4.10 i386" `I386 v4_10
      ~expect_exists:false ~expected:["domains"; "multicore"];
    test "Multicore not on 4.12 xi386" `I386 v4_12
      ~expect_exists:false ~expected:["domains"; "multicore"]
  ])

let test_compiler_arches_upper =
  let test name arch version ~expected =
    ( name,
      `Quick,
      fun () ->
        let has = Ocaml_version.Has.arch arch version in
        Alcotest.(check bool __LOC__ has expected) )
  in
  Ocaml_version.Releases.([
    test "x86_64 latest supported" `X86_64 latest ~expected:true;
    test "aarch64 latest supported" `Aarch64 latest ~expected:true;
    test "i386 latest unsupported" `I386 latest ~expected:false;
    test "aarch32 latest unsupported" `Aarch32 latest ~expected:false;
    test "ppc64le latest unsupported" `Ppc64le latest ~expected:false;
    test "s390x latest unsupported" `S390x latest ~expected:false;
    test "riscv64 latest unsupported" `Riscv64 latest ~expected:false;
    test "i386 5.0 unsupported" `I386 v5_0_0 ~expected:false;
    test "aarch32 5.0 unsupported" `Aarch32 v5_0_0 ~expected:false;
    test "ppc64le 5.0 unsupported" `Ppc64le v5_0_0 ~expected:false;
    test "s390x 5.0 unsupported" `S390x v5_0_0 ~expected:false;
    test "riscv64 5.0 unsupported" `Riscv64 v5_0_0 ~expected:false;
    test "i386 4.14 supported" `I386 v4_14 ~expected:true;
    test "aarch32 4.14 supported" `Aarch32 v4_14 ~expected:true;
    test "ppc64le 4.14 supported" `Ppc64le v4_14 ~expected:true;
    test "s390x 4.14 supported" `S390x v4_14 ~expected:true;
    test "riscv64 4.14 supported" `Riscv64 v4_14 ~expected:true;
  ])

let () =
  Alcotest.run "ocaml-version" [
    ("Compiler_variants", test_compiler_variants);
    ("Configure_options", test_configure_options);
    ("Compiler_arches_upper", test_compiler_arches_upper)
  ]
