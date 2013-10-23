package com.example

import org.specs._

import dispatch.classic.Http

object ExampleSpec extends Specification with unfiltered.spec.jetty.Served {

  def setup = { _.filter(new App) }

  val http = new Http
  
  "The example app" should {
    "serve unfiltered requests" in {
      val status = http x (host as_str) {
        case (code, _, _, _) => code
      }
      status must_== 200
    }
  }
}
