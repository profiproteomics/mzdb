package fr.profi.chemistry.model

package object implicits {
  
  private[this] object CharProperties {
    val ASCIILetters = ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  }
  
  implicit class CharProperties(val ch: Char) extends AnyVal {
    def isASCIILetter: Boolean = CharProperties.ASCIILetters.contains(ch)
  }

}