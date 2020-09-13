object CaseConverter extends App {

  def camelcase(s: String): String = {
    if(s.contains("_")){
      if (s.head == '_') {
        "_".concat((s.tail.split("_").toList match {
          case head :: tail => head :: tail.map(_.capitalize)
          case x => x
        }).mkString)
      }
      else {
        (s.split("_").toList match {
          case head :: tail => head :: tail.map(_.capitalize)
          case x => x
        }).mkString
      }
    }
    else {
      s //As the String is in normal form, i.e. doesnot contain `_`. So return same string
    }

  }

  def snakecase(s: String): String = s.foldLeft(new StringBuilder) {
    case (s, c) if Character.isUpperCase(c) =>
      s.append("_").append(Character.toLowerCase(c))
    case (s, c) =>
      s.append(c)
  }.toString

}
