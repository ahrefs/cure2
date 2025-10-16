type charset

type t

val to_re2 : t -> Re2.t

val to_string : t -> string

val str : string -> t

val char : char -> t

(** {2 Basic operations on regular expressions} *)

val alt : t list -> t
(** Alternative.

    [alt []] is equivalent to {!empty}.

    The leftmost match is preferred. *)

val ( || ) : t -> t -> t
(** [(||) x y ] is [alt [x; y]] *)

val seq : t list -> t
(** Sequence *)

val ( + ) : t -> t -> t
(** [(+) x y ] is [seq [x; y]] *)

val rep : ?min:int -> ?max:int -> t -> t
(** [rep ~min ~max re] matches [re] at least [min] times
    and at most [max] times, bounds included.
    [min] defaults to 0 and [max] to infinity.  *)

val rep1 : t -> t
(** 1 or more matches *)

val opt : t -> t
(** 0 or 1 matches *)

(** {2 String, line, word} *)

val start : t
(** Initial position *)

val stop : t
(** Final position *)

val bol : t
(** Beginning of line, compiled to ["^"] *)

val eol : t
(** End of line, compiled to ["$"] *)

val bow : t
(** Boundary of (ascii) word *)

val not_bow : t
(** Not at a boundary of (ascii) word *)

val whole_string : t -> t
(** Only matches the whole string, i.e. [fun t -> seq [ bos; t; eos ]]. *)

(** {2 Semantics}*)

val greedy : t -> t

val non_greedy : t -> t

val case : t -> t
(** Case sensitive matching. On by default *)

val no_case : t -> t
(** Case insensitive matching. Off by default/*)

val group : ?name:string -> t -> t

(** {2 Charsets} *)

val any : t
(** any character, including newline. To exclude newline, use {!notnl} *)

val notnl : t
(** any character except newline. *)

val alnum : t
(** ascii alphanumeric ( [0-9A-Za-z])*)

val alpha : t
(** ascii alphabetic ( [A-Za-z])*)

val ascii : t
(** ascii ASCII ( [\x00-\x7F])*)

val blank : t
(** ascii blank ( [\t ])*)

val cntrl : t
(** ascii control ( [\x00-\x1F\x7F])*)

val digit : t
(** ascii digits ( [0-9])*)

val graph : t
(** ascii graphical ( [!-~]  [A-Za-z0-9!""#$%&'()*+,\-./:;<=>?@[\\\]^_`{}|~])*)

val lower : t
(** ascii lower case ( [a-z])*)

val print : t
(** ascii printable ( [ -~]  [ [:graph:]])*)

val punct : t
(** ascii punctuation ( [!-/:-@[-`{-~])*)

val space : t
(** ascii whitespace ( [\t\n\v\f\r ])*)

val upper : t
(** ascii upper case ( [A-Z])*)

val word : t
(** ascii word characters ( [0-9A-Za-z_])*)

val xdigit : t
(**	ascii hex digit ( [0-9A-Fa-f])*)

val not_alnum : t
(** not ascii alphanumeric ( [0-9A-Za-z])*)

val not_alpha : t
(** not ascii alphabetic ( [A-Za-z])*)

val not_ascii : t
(** not ascii ASCII ( [\x00-\x7F])*)

val not_blank : t
(** not ascii blank ( [\t ])*)

val not_cntrl : t
(** not ascii control ( [\x00-\x1F\x7F])*)

val not_digit : t
(** not ascii digits ( [0-9])*)

val not_graph : t
(** not ascii graphical ( [!-~]  [A-Za-z0-9!""#$%&'()*+,\-./:;<=>?@[\\\]^_`{}|~])*)

val not_lower : t
(** not ascii lower case ( [a-z])*)

val not_print : t
(** not ascii printable ( [ -~]  [ [:graph:]])*)

val not_punct : t
(** not ascii punctuation ( [!-/:-@[-`{-~])*)

val not_space : t
(** not ascii whitespace ( [\t\n\v\f\r ])*)

val not_upper : t
(** not ascii upper case ( [A-Z])*)

val not_word : t
(** not ascii word characters ( [0-9A-Za-z_])*)

val not_xdigit : t
(**	not ascii hex digit ( [0-9A-Fa-f])*)

val chars : string -> t
(** any character of the string *)

val charset : ?neg:bool -> charset list -> t
(** [charset cs] matches any character that is part of [cs]
    [charset ~neg:true cs] matches any character that is not part of [cs] *)

module Charset : sig
  val range : char -> char -> charset

  val single : char -> charset

  val chars : string -> charset

  module Perl : sig
    val digit : charset

    val not_digit : charset

    val whitespace : charset

    val not_whitespace : charset

    val word : charset

    val not_word : charset
  end

  module Ascii : sig
    val alnum : charset
    (** alphanumeric ( [0-9A-Za-z])*)

    val alpha : charset
    (** alphabetic ( [A-Za-z])*)

    val ascii : charset
    (** ASCII ( [\x00-\x7F])*)

    val blank : charset
    (** blank ( [\t ])*)

    val cntrl : charset
    (** control ( [\x00-\x1F\x7F])*)

    val digit : charset
    (** digits ( [0-9])*)

    val graph : charset
    (** graphical ( [!-~]  [A-Za-z0-9!""#$%&'()*+,\-./:;<=>?@[\\\]^_`{}|~])*)

    val lower : charset
    (** lower case ( [a-z])*)

    val print : charset
    (** printable ( [ -~]  [ [:graph:]])*)

    val punct : charset
    (** punctuation ( [!-/:-@[-`{-~])*)

    val space : charset
    (** whitespace ( [\t\n\v\f\r ])*)

    val upper : charset
    (** upper case ( [A-Z])*)

    val word : charset
    (** word characters ( [0-9A-Za-z_])*)

    val xdigit : charset
    (**	hex digit ( [0-9A-Fa-f])*)
  end

  module Unicode : sig
    val other : charset

    val control : charset

    val format : charset

    val private_use : charset

    val surrogate : charset

    val letter : charset

    val lowercase_letter : charset

    val modifier_letter : charset

    val other_letter : charset

    val titlecase_letter : charset

    val uppercase_letter : charset

    val mark : charset

    val spacing_mark : charset

    val enclosing_mark : charset

    val non_spacing_mark : charset

    val number : charset

    val decimal_number : charset

    val letter_number : charset

    val other_number : charset

    val punctuation : charset

    val connector_punctuation : charset

    val dash_punctuation : charset

    val close_punctuation : charset

    val final_punctuation : charset

    val initial_punctuation : charset

    val other_punctuation : charset

    val open_punctuation : charset

    val symbol : charset

    val currency_symbol : charset

    val modifier_symbol : charset

    val math_symbol : charset

    val other_symbol : charset

    val separator : charset

    val line_separator : charset

    val paragraph_separator : charset

    val space_separator : charset

    (** {2 Scripts} *)

    val adlam : charset

    val ahom : charset

    val anatolian_hieroglyphs : charset

    val arabic : charset

    val armenian : charset

    val avestan : charset

    val balinese : charset

    val bamum : charset

    val bassa_Vah : charset

    val batak : charset

    val bengali : charset

    val bhaiksuki : charset

    val bopomofo : charset

    val brahmi : charset

    val braille : charset

    val buginese : charset

    val buhid : charset

    val canadian_Aboriginal : charset

    val carian : charset

    val caucasian_Albanian : charset

    val chakma : charset

    val cham : charset

    val cherokee : charset

    val chorasmian : charset

    val common : charset

    val coptic : charset

    val cuneiform : charset

    val cypriot : charset

    val cypro_Minoan : charset

    val cyrillic : charset

    val deseret : charset

    val devanagari : charset

    val dives_Akuru : charset

    val dogra : charset

    val duployan : charset

    val egyptian_Hieroglyphs : charset

    val elbasan : charset

    val elymaic : charset

    val ethiopic : charset

    val georgian : charset

    val glagolitic : charset

    val gothic : charset

    val grantha : charset

    val greek : charset

    val gujarati : charset

    val gunjala_Gondi : charset

    val gurmukhi : charset

    val han : charset

    val hangul : charset

    val hanifi_Rohingya : charset

    val hanunoo : charset

    val hatran : charset

    val hebrew : charset

    val hiragana : charset

    val imperial_Aramaic : charset

    val inherited : charset

    val inscriptional_Pahlavi : charset

    val inscriptional_Parthian : charset

    val javanese : charset

    val kaithi : charset

    val kannada : charset

    val katakana : charset

    val kawi : charset

    val kayah_Li : charset

    val kharoshthi : charset

    val khitan_Small_Script : charset

    val khmer : charset

    val khojki : charset

    val khudawadi : charset

    val lao : charset

    val latin : charset

    val lepcha : charset

    val limbu : charset

    val linear_A : charset

    val linear_B : charset

    val lisu : charset

    val lycian : charset

    val lydian : charset

    val mahajani : charset

    val makasar : charset

    val malayalam : charset

    val mandaic : charset

    val manichaean : charset

    val marchen : charset

    val masaram_gondi : charset

    val medefaidrin : charset

    val meetei_gayek : charset

    val mende_gikakui : charset

    val meroitic_cursive : charset

    val meroitic_hieroglyphs : charset

    val miao : charset

    val modi : charset

    val mongolian : charset

    val mro : charset

    val multani : charset

    val myanmar : charset

    val nabataean : charset

    val nag_mundari : charset

    val nandinagari : charset

    val new_Tai_Lue : charset

    val newa : charset

    val nko : charset

    val nushu : charset

    val nyiakeng_puachue_hmong : charset

    val ogham : charset

    val ol_Chiki : charset

    val old_Hungarian : charset

    val old_Italic : charset

    val old_North_Arabian : charset

    val old_Permic : charset

    val old_Persian : charset

    val old_Sogdian : charset

    val old_South_Arabian : charset

    val old_Turkic : charset

    val old_Uyghur : charset

    val oriya : charset

    val osage : charset

    val osmanya : charset

    val pahawh_Hmong : charset

    val palmyrene : charset

    val pau_Cin_Hau : charset

    val phags_Pa : charset

    val phoenician : charset

    val psalter_Pahlavi : charset

    val rejang : charset

    val runic : charset

    val samaritan : charset

    val saurashtra : charset

    val sharada : charset

    val shavian : charset

    val siddham : charset

    val signWriting : charset

    val sinhala : charset

    val sogdian : charset

    val sora_Sompeng : charset

    val soyombo : charset

    val sundanese : charset

    val syloti_Nagri : charset

    val syriac : charset

    val tagalog : charset

    val tagbanwa : charset

    val tai_Le : charset

    val tai_Tham : charset

    val tai_Viet : charset

    val takri : charset

    val tamil : charset

    val tangsa : charset

    val tangut : charset

    val telugu : charset

    val thaana : charset

    val thai : charset

    val tibetan : charset

    val tifinagh : charset

    val tirhuta : charset

    val toto : charset

    val ugaritic : charset

    val vai : charset

    val vithkuqi : charset

    val wancho : charset

    val warang_Citi : charset

    val yezidi : charset

    val yi : charset

    val zanabazar_Square : charset
  end
end
