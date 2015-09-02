package main.scala

import scalaj.http.Http

/**
 * @author Anton Bogushevsky
 */
object Application extends App {
  println("Hello world!")
  
  val html = Http("https://mail.ru/").asString
  val parser = SimpleHtmlParser(html)
}