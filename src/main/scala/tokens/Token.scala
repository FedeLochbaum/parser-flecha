package tokens

abstract class Token

case class EOFToken() extends Token // Final token

// Constants
case class LOWERIDToken(value: String) extends Token

case class UPPERIDToken(value: String) extends Token

case class NUMBERToken(value: Int) extends Token

case class CHARToken(value: String) extends Token

case class STRINGToken(value: String) extends Token

// Keywords
case class DEFToken() extends Token       // def

case class IFToken() extends Token        // if

case class THENToken() extends Token      // then

case class ELIFToken() extends Token      // elif

case class ELSEToken() extends Token      // else

case class CASEToken() extends Token      // case

case class LETToken() extends Token       // let

case class INToken() extends Token        // in

// Limiters
case class DEFEQToken() extends Token     // =

case class SEMICOLONToken() extends Token // ;

case class LPARENToken() extends Token    // (

case class RPARENToken() extends Token    // )

case class LAMBDAToken() extends Token    // \

case class PIPEToken() extends Token      // |

case class ARROWToken() extends Token     // ->

// Logical operators
case class ANDToken() extends Token       // &&

case class ORToken() extends Token        // ||

case class NOTToken() extends Token       // !

// Relational operators
case class EQToken() extends Token        // ==

case class NEToken() extends Token        // !=

case class GEToken() extends Token        // >=

case class LEToken() extends Token        // <=

case class GTToken() extends Token        // >

case class LTToken() extends Token        // <

// Arithmetic operators
case class PLUSToken() extends Token      // +

case class MINUSToken() extends Token     // -

case class TIMESToken() extends Token     // *

case class DIVToken() extends Token       // /

case class MODToken() extends Token       // %
