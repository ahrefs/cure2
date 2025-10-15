type t = (Cure2.t * (string * bool) list) list

let test_regexp (tests : t) =
  tests
  |> List.map (fun (re, tests) ->
         let test () =
           tests
           |> List.iter (fun (input, is_match) ->
                  let re = Cure2.(whole_string re) in
                  let re_str = Cure2.to_string re in
                  let re2 = Cure2.to_re2 re in
                  Alcotest.(check bool)
                    (Printf.sprintf {|%S ~ "%s"|} input re_str)
                    is_match (Re2.matches re2 input) )
         in
         let name = {|"|} ^ Cure2.to_string re ^ {|"|} in
         Alcotest.test_case name `Quick test )

let tests_instance : t =
  let open Cure2 in
  [ ( rep any
    , [ ("foo", true)
      ; ("", true)
      ; ("bar", true)
      ; ("akshfao24534 fd s f43 \n spof f \t", true) ] )
  ; ( rep notnl
    , [ ("foo", true)
      ; ("", true)
      ; ("bar", true)
      ; ("ba\nr", false)
      ; ("akshfao24534 fd s f43 \n spof f \t", false) ] )
  ; ( rep (char 'c')
    , [ ("cccccc", true)
      ; ("cccccbccc", false)
      ; ("akshfaco24534 fd s f43 \n spof f \t", false) ] )
  ; ( alt [char 'a'; char 'b']
    , [("a", true); ("b", true); ("c", false); ("aa", false); ("bb", false)] )
  ; ( alt [rep (char 'a'); rep (char 'b')]
    , [ ("aaaaaa", true)
      ; ("bbbbbbb", true)
      ; ("cccccccc", false)
      ; ("aaaaaabbbbb", false) ] )
  ; ( alt [rep (char 'c'); seq [rep (char 'c'); char 'b'; rep (char 'c')]]
    , [ ("cccccc", true)
      ; ("cccccbccc", true)
      ; ("aaaa", false) (*; ("akshfaco24534 fd s f43 \n spof f \t", false)*) ]
    )
  ; (* Empty regex *)
    ( seq [rep any; bow; str "aabb"]
    , [ ("ccc aabb", true)
      ; ("   aabb", true)
      ; ("ccccaabb", false)
      ; ("aabb", true) ] )
  ; ( seq [rep any; bol; str "aabb"]
    , [ ("cccc\naabb", true)
      ; ("   aabb", false)
      ; ("ccccaabb", false)
      ; ("aabb", true) ] )
  ; (* Flags *)
    ( seq [rep (str "a"); no_case (seq [rep (str "b"); case (rep (str "c"))])]
    , [ ("aaaabbbccc", true)
      ; ("aaAabbbccc", false)
      ; ("aaaabBbccc", true)
      ; ("aaaabbbcCc", false) ] )
  ; (* Charsets *)
    ( rep (charset Charset.[single 'a'])
    , [("aaaa", true); ("bbbbb", false); ("aaaaabaa", false)] )
  ; ( rep (charset ~neg:true Charset.[single 'a'])
    , [("aaaa", false); ("bbbbb", true); ("aaaaabaa", false)] )
  ; ( rep (charset Charset.[single '^'])
    , [("^", true); ("bbbbb", false); ("^^^^^^b^^", false)] )
  ; ( rep (charset Charset.[single '['])
    , [("[", true); ("bbbbb", false); ("[[[[[[b[[", false)] )
  ; ( rep (charset Charset.[single ']'])
    , [("]", true); ("bbbbb", false); ("]]]]]]b]]", false)] )
  ; ( rep (charset Charset.[single 'a'; single '^'; single ']'])
    , [ ("aaaaaaaa", true)
      ; ("^^^", true)
      ; ("]]]", true)
      ; ("aaaaa]]]aa^^a", true)
      ; ("bbbbb", false)
      ; ("aaaaa]]]aa^^ab", false)
      ; ("]]]]]]b]]", false) ] )
  ; ( rep (charset ~neg:true Charset.[single 'a'; single '^'; single ']'])
    , [ ("aaaaaaaa", false)
      ; ("^^^", false)
      ; ("]]]", false)
      ; ("aaaaa]]]aa^^a", false)
      ; ("bbbbb", true)
      ; ("aaaaa]]]aa^^ab", false)
      ; ("]]]]]]b]]", false) ] )
  ; ( rep (charset Charset.[single 'a'; single '-'; single 'z'])
    , [("aaa", true); ("zzz", true); ("---", true); ("bcd", false)] )
  ; ( rep (charset Charset.[Ascii.alnum])
    , [("abc123", true); (".", false); ("]]]]]]b]]", false)] )
  ; ( rep (charset Charset.[Ascii.alnum; single '.'])
    , [("abc123", true); ("abc123...", true); (".", true); ("]]]]]]b]]", false)]
    )
  ; ( rep (charset Charset.[Ascii.alnum; Ascii.punct])
    , [("abc123", true); ("abc123...", true); (".", true); ("]]]]]]b]] ", false)]
    )
  ; ( opt (chars "+-")
      + ((rep1 digit + opt (char '.' + rep digit)) || (char '.' + rep1 digit))
      + opt (chars "eE" + opt (chars "+-") + rep1 digit)
    , [ ("12345", true)
      ; ("12345.5", true)
      ; ("12345e+123", true)
      ; ("-12345e+123", true)
      ; (".12345e+123", true)
      ; ("+.12345e+123", true)
      ; ("-.12345e+123", true)
      ; ("+12345e+123", true)
      ; ("12345e12", true)
      ; ("12345e-12", true)
      ; ("12345E-12", true)
      ; ("12345.234E-12", true) ] ) ]

let () =
  let open Alcotest in
  run "Cure2" [("predefined regexps", test_regexp tests_instance)]
