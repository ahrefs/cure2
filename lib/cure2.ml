type charset =
  | Ascii of string
  | Unicode of string
  | Range of char * char
  | Single of char
  | Perl of string
  | Chars of string

type t =
  | Charset of bool * charset list
  | Seq of t list
  | Alt of t list
  | Rep of {re: t; min: int; max: int option}
  | Regex of string
  | String of string
  | Flags of string * t
  | Group of string option * t

let is_simple = function
  | Group _ | Charset _ | Flags _ ->
      true
  | (Regex s | String s) when String.length s = 1 ->
      true
  | _ ->
      false

let charset_escape = function
  | '^' ->
      {|\^|}
  | '-' ->
      {|\-|}
  | '[' ->
      {|\[|}
  | ']' ->
      {|\]|}
  | '\n' ->
      {|\n|}
  | '\t' ->
      {|\t|}
  | c ->
      String.make 1 c

let rec flatten_alts = function
  | Alt li ->
      Alt
        ( li |> List.map flatten_alts
        |> List.concat_map (function Alt li -> li | x -> [x]) )
  | Seq li ->
      Seq (li |> List.map flatten_alts)
  | Rep {re; min; max} ->
      Rep {re= flatten_alts re; min; max}
  | Flags (flags, re) ->
      Flags (flags, flatten_alts re)
  | Group (name, re) ->
      Group (name, flatten_alts re)
  | (Charset _ | Regex _ | String _) as x ->
      x

let to_buffer buf t =
  let t = flatten_alts t in
  let print str = Buffer.add_string buf str in
  let rec loop ~paren_alt = function
    | Seq li ->
        List.iter (loop ~paren_alt:true) li
    | Alt li ->
        if paren_alt then print "(?:" ;
        let rec loop_alt = function
          | elt :: elt' :: li ->
              (loop ~paren_alt:false) elt ;
              print "|" ;
              loop_alt (elt' :: li)
          | [elt] ->
              (loop ~paren_alt:true) elt
          | [] ->
              ()
        in
        loop_alt li ;
        if paren_alt then print ")"
    | Rep {re; min= 0; max= None} ->
        paren re ; print "*"
    | Rep {re; min= 1; max= None} ->
        paren re ; print "+"
    | Rep {re; min= 0; max= Some 1} ->
        paren re ; print "?"
    | Rep {re; min; max= Some max} when min = max ->
        paren re ;
        print "{" ;
        print (string_of_int min) ;
        print "}"
    | Rep {re; min; max} ->
        paren re ;
        print "{" ;
        print (string_of_int min) ;
        print "," ;
        max |> Option.iter (fun max -> print (string_of_int max)) ;
        print "}"
    | Regex str ->
        print str
    | String str ->
        print (Re2.escape str)
    | Flags (flags, t) ->
        print "(?" ;
        print flags ;
        print ":" ;
        loop ~paren_alt:false t ;
        print ")"
    | Group (name, t) ->
        print "(" ;
        Option.iter (fun name -> print "?P<" ; print name ; print ">") name ;
        loop ~paren_alt:false t ;
        print ")"
    | Charset (neg, cs) ->
        print "[" ;
        if neg then print "^" ;
        cs
        |> List.iter (function
             | Single c ->
                 print (charset_escape c)
             | Ascii name ->
                 print "[:" ; print name ; print ":]"
             | Perl name ->
                 print {|\|} ; print name
             | Unicode name ->
                 print {|\p{|} ; print name ; print "}"
             | Range (lo, hi) ->
                 print (charset_escape lo) ;
                 print "-" ;
                 print (charset_escape hi)
             | Chars str ->
                 str |> String.iter (fun c -> print (charset_escape c)) ) ;
        print "]"
  and paren t =
    if is_simple t then loop ~paren_alt:true t
    else (print "(?:" ; loop ~paren_alt:false t ; print ")")
  in
  loop ~paren_alt:true (Flags ("ms", t))

let to_string t =
  let buf = Buffer.create 16 in
  to_buffer buf t ; Buffer.contents buf

let to_re2 t = t |> to_string |> Re2.create_exn

let regex str = Regex str

let str s = String s

let char c = String (String.make 1 c)

let seq li = Seq li

let ( + ) x y = seq [x; y]

let alt li = Alt li

let ( || ) x y = alt [x; y]

let rep ?(min = 0) ?max re = Rep {re; min; max}

let ( !* ) x = rep x

let rep1 t = rep ~min:1 t

let ( !+ ) = rep1

let opt t = rep ~min:0 ~max:1 t

let ( !? ) = opt

let any = regex "."

let notnl = Charset (true, [Single '\n'])

let greedy t = Flags ("-U", t)

let non_greedy t = Flags ("U", t)

let case t = Flags ("-i", t)

let no_case t = Flags ("i", t)

let bol = regex {|^|}

let eol = regex {|$w|}

let bow = regex {|\b|}

let not_bow = regex {|\B|}

let start = regex {|\A|}

let stop = regex {|\z|}

let whole_string t = seq [start; t; stop]

let group ?name t = Group (name, t)

let charset ?(neg = false) cs = Charset (neg, cs)

module Charset = struct
  let range a b = Range (a, b)

  let chars str = Chars str

  let single c = Single c

  module Perl = struct
    let digit = Perl "d"

    let not_digit = Perl "D"

    let whitespace = Perl "s"

    let not_whitespace = Perl "S"

    let word = Perl "w"

    let not_word = Perl "W"
  end

  module Ascii = struct
    let alnum = Ascii "alnum"

    let alpha = Ascii "alpha"

    let ascii = Ascii "ascii"

    let blank = Ascii "blank"

    let cntrl = Ascii "cntrl"

    let digit = Ascii "digit"

    let graph = Ascii "graph"

    let lower = Ascii "lower"

    let print = Ascii "print"

    let punct = Ascii "punct"

    let space = Ascii "space"

    let upper = Ascii "upper"

    let word = Ascii "word"

    let xdigit = Ascii "xdigit"
  end

  module Unicode = struct
    let other = Unicode "C"

    let control = Unicode "Cc"

    let format = Unicode "Cf"

    let private_use = Unicode "Co"

    let surrogate = Unicode "Cs"

    let letter = Unicode "L"

    let lowercase_letter = Unicode "Ll"

    let modifier_letter = Unicode "Lm"

    let other_letter = Unicode "Lo"

    let titlecase_letter = Unicode "Lt"

    let uppercase_letter = Unicode "Lu"

    let mark = Unicode "M"

    let spacing_mark = Unicode "Mc"

    let enclosing_mark = Unicode "Me"

    let non_spacing_mark = Unicode "Mn"

    let number = Unicode "N"

    let decimal_number = Unicode "Nd"

    let letter_number = Unicode "Nl"

    let other_number = Unicode "No"

    let punctuation = Unicode "P"

    let connector_punctuation = Unicode "Pc"

    let dash_punctuation = Unicode "Pd"

    let close_punctuation = Unicode "Pe"

    let final_punctuation = Unicode "Pf"

    let initial_punctuation = Unicode "Pi"

    let other_punctuation = Unicode "Po"

    let open_punctuation = Unicode "Ps"

    let symbol = Unicode "S"

    let currency_symbol = Unicode "Sc"

    let modifier_symbol = Unicode "Sk"

    let math_symbol = Unicode "Sm"

    let other_symbol = Unicode "So"

    let separator = Unicode "Z"

    let line_separator = Unicode "Zl"

    let paragraph_separator = Unicode "Zp"

    let space_separator = Unicode "Zs"

    let adlam = Unicode "Adlam"

    let ahom = Unicode "Ahom"

    let anatolian_hieroglyphs = Unicode "Anatolian_Hieroglyphs"

    let arabic = Unicode "Arabic"

    let armenian = Unicode "Armenian"

    let avestan = Unicode "Avestan"

    let balinese = Unicode "Balinese"

    let bamum = Unicode "Bamum"

    let bassa_Vah = Unicode "Bassa_Vah"

    let batak = Unicode "Batak"

    let bengali = Unicode "Bengali"

    let bhaiksuki = Unicode "Bhaiksuki"

    let bopomofo = Unicode "Bopomofo"

    let brahmi = Unicode "Brahmi"

    let braille = Unicode "Braille"

    let buginese = Unicode "Buginese"

    let buhid = Unicode "Buhid"

    let canadian_Aboriginal = Unicode "Canadian_Aboriginal"

    let carian = Unicode "Carian"

    let caucasian_Albanian = Unicode "Caucasian_Albanian"

    let chakma = Unicode "Chakma"

    let cham = Unicode "Cham"

    let cherokee = Unicode "Cherokee"

    let chorasmian = Unicode "Chorasmian"

    let common = Unicode "Common"

    let coptic = Unicode "Coptic"

    let cuneiform = Unicode "Cuneiform"

    let cypriot = Unicode "Cypriot"

    let cypro_Minoan = Unicode "Cypro_Minoan"

    let cyrillic = Unicode "Cyrillic"

    let deseret = Unicode "Deseret"

    let devanagari = Unicode "Devanagari"

    let dives_Akuru = Unicode "Dives_Akuru"

    let dogra = Unicode "Dogra"

    let duployan = Unicode "Duployan"

    let egyptian_Hieroglyphs = Unicode "Egyptian_Hieroglyphs"

    let elbasan = Unicode "Elbasan"

    let elymaic = Unicode "Elymaic"

    let ethiopic = Unicode "Ethiopic"

    let georgian = Unicode "Georgian"

    let glagolitic = Unicode "Glagolitic"

    let gothic = Unicode "Gothic"

    let grantha = Unicode "Grantha"

    let greek = Unicode "Greek"

    let gujarati = Unicode "Gujarati"

    let gunjala_Gondi = Unicode "Gunjala_Gondi"

    let gurmukhi = Unicode "Gurmukhi"

    let han = Unicode "Han"

    let hangul = Unicode "Hangul"

    let hanifi_Rohingya = Unicode "Hanifi_Rohingya"

    let hanunoo = Unicode "Hanunoo"

    let hatran = Unicode "Hatran"

    let hebrew = Unicode "Hebrew"

    let hiragana = Unicode "Hiragana"

    let imperial_Aramaic = Unicode "Imperial_Aramaic"

    let inherited = Unicode "Inherited"

    let inscriptional_Pahlavi = Unicode "Inscriptional_Pahlavi"

    let inscriptional_Parthian = Unicode "Inscriptional_Parthian"

    let javanese = Unicode "Javanese"

    let kaithi = Unicode "Kaithi"

    let kannada = Unicode "Kannada"

    let katakana = Unicode "Katakana"

    let kawi = Unicode "Kawi"

    let kayah_Li = Unicode "Kayah_Li"

    let kharoshthi = Unicode "Kharoshthi"

    let khitan_Small_Script = Unicode "Khitan_Small_Script"

    let khmer = Unicode "Khmer"

    let khojki = Unicode "Khojki"

    let khudawadi = Unicode "Khudawadi"

    let lao = Unicode "Lao"

    let latin = Unicode "Latin"

    let lepcha = Unicode "Lepcha"

    let limbu = Unicode "Limbu"

    let linear_A = Unicode "Linear_A"

    let linear_B = Unicode "Linear_B"

    let lisu = Unicode "Lisu"

    let lycian = Unicode "Lycian"

    let lydian = Unicode "Lydian"

    let mahajani = Unicode "Mahajani"

    let makasar = Unicode "Makasar"

    let malayalam = Unicode "Malayalam"

    let mandaic = Unicode "Mandaic"

    let manichaean = Unicode "Manichaean"

    let marchen = Unicode "Marchen"

    let masaram_gondi = Unicode "Masaram_Gondi"

    let medefaidrin = Unicode "Medefaidrin"

    let meetei_gayek = Unicode "Meetei_Mayek"

    let mende_gikakui = Unicode "Mende_Kikakui"

    let meroitic_cursive = Unicode "Meroitic_Cursive"

    let meroitic_hieroglyphs = Unicode "Meroitic_Hieroglyphs"

    let miao = Unicode "Miao"

    let modi = Unicode "Modi"

    let mongolian = Unicode "Mongolian"

    let mro = Unicode "Mro"

    let multani = Unicode "Multani"

    let myanmar = Unicode "Myanmar"

    let nabataean = Unicode "Nabataean"

    let nag_mundari = Unicode "Nag_Mundari"

    let nandinagari = Unicode "Nandinagari"

    let new_Tai_Lue = Unicode "New_Tai_Lue"

    let newa = Unicode "Newa"

    let nko = Unicode "Nko"

    let nushu = Unicode "Nushu"

    let nyiakeng_puachue_hmong = Unicode "Nyiakeng_Puachue_Hmong"

    let ogham = Unicode "Ogham"

    let ol_Chiki = Unicode "Ol_Chiki"

    let old_Hungarian = Unicode "Old_Hungarian"

    let old_Italic = Unicode "Old_Italic"

    let old_North_Arabian = Unicode "Old_North_Arabian"

    let old_Permic = Unicode "Old_Permic"

    let old_Persian = Unicode "Old_Persian"

    let old_Sogdian = Unicode "Old_Sogdian"

    let old_South_Arabian = Unicode "Old_South_Arabian"

    let old_Turkic = Unicode "Old_Turkic"

    let old_Uyghur = Unicode "Old_Uyghur"

    let oriya = Unicode "Oriya"

    let osage = Unicode "Osage"

    let osmanya = Unicode "Osmanya"

    let pahawh_Hmong = Unicode "Pahawh_Hmong"

    let palmyrene = Unicode "Palmyrene"

    let pau_Cin_Hau = Unicode "Pau_Cin_Hau"

    let phags_Pa = Unicode "Phags_Pa"

    let phoenician = Unicode "Phoenician"

    let psalter_Pahlavi = Unicode "Psalter_Pahlavi"

    let rejang = Unicode "Rejang"

    let runic = Unicode "Runic"

    let samaritan = Unicode "Samaritan"

    let saurashtra = Unicode "Saurashtra"

    let sharada = Unicode "Sharada"

    let shavian = Unicode "Shavian"

    let siddham = Unicode "Siddham"

    let signWriting = Unicode "SignWriting"

    let sinhala = Unicode "Sinhala"

    let sogdian = Unicode "Sogdian"

    let sora_Sompeng = Unicode "Sora_Sompeng"

    let soyombo = Unicode "Soyombo"

    let sundanese = Unicode "Sundanese"

    let syloti_Nagri = Unicode "Syloti_Nagri"

    let syriac = Unicode "Syriac"

    let tagalog = Unicode "Tagalog"

    let tagbanwa = Unicode "Tagbanwa"

    let tai_Le = Unicode "Tai_Le"

    let tai_Tham = Unicode "Tai_Tham"

    let tai_Viet = Unicode "Tai_Viet"

    let takri = Unicode "Takri"

    let tamil = Unicode "Tamil"

    let tangsa = Unicode "Tangsa"

    let tangut = Unicode "Tangut"

    let telugu = Unicode "Telugu"

    let thaana = Unicode "Thaana"

    let thai = Unicode "Thai"

    let tibetan = Unicode "Tibetan"

    let tifinagh = Unicode "Tifinagh"

    let tirhuta = Unicode "Tirhuta"

    let toto = Unicode "Toto"

    let ugaritic = Unicode "Ugaritic"

    let vai = Unicode "Vai"

    let vithkuqi = Unicode "Vithkuqi"

    let wancho = Unicode "Wancho"

    let warang_Citi = Unicode "Warang_Citi"

    let yezidi = Unicode "Yezidi"

    let yi = Unicode "Yi"

    let zanabazar_Square = Unicode "Zanabazar_Square"
  end
end

let alnum = charset [Charset.Ascii.alnum]

let alpha = charset [Charset.Ascii.alpha]

let ascii = charset [Charset.Ascii.ascii]

let blank = charset [Charset.Ascii.blank]

let cntrl = charset [Charset.Ascii.cntrl]

let digit = charset [Charset.Ascii.digit]

let graph = charset [Charset.Ascii.graph]

let lower = charset [Charset.Ascii.lower]

let print = charset [Charset.Ascii.print]

let punct = charset [Charset.Ascii.punct]

let space = charset [Charset.Ascii.space]

let upper = charset [Charset.Ascii.upper]

let word = charset [Charset.Ascii.word]

let xdigit = charset [Charset.Ascii.xdigit]

let not_alnum = charset ~neg:true [Charset.Ascii.alnum]

let not_alpha = charset ~neg:true [Charset.Ascii.alpha]

let not_ascii = charset ~neg:true [Charset.Ascii.ascii]

let not_blank = charset ~neg:true [Charset.Ascii.blank]

let not_cntrl = charset ~neg:true [Charset.Ascii.cntrl]

let not_digit = charset ~neg:true [Charset.Ascii.digit]

let not_graph = charset ~neg:true [Charset.Ascii.graph]

let not_lower = charset ~neg:true [Charset.Ascii.lower]

let not_print = charset ~neg:true [Charset.Ascii.print]

let not_punct = charset ~neg:true [Charset.Ascii.punct]

let not_space = charset ~neg:true [Charset.Ascii.space]

let not_upper = charset ~neg:true [Charset.Ascii.upper]

let not_word = charset ~neg:true [Charset.Ascii.word]

let not_xdigit = charset ~neg:true [Charset.Ascii.xdigit]

let chars str = charset [Charset.chars str]
