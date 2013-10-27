package tsrv

import org.specs._

import org.json4s._
import org.json4s.native.Serialization.read
import scalaj.http.{HttpOptions, Http}

object ServerSpec extends Specification with unfiltered.spec.jetty.Served {
  implicit val formats = native.Serialization.formats(NoTypeHints)

  def setup = { _.context("/js") {
    _.resources(new java.net.URL(getClass().getResource("/www/js/t.js"), "."))
  }.filter(new App) }

  "Client interface" should {
    "return a record" in {
      val result = Http.get(s"${server.url}record").asString
      val expected = Tournament(id = 123, teams = List("A", "B"))
      read[Tournament](result) must_== expected
    }

    "store a record" in {
      val result = Http.post(s"${server.url}record").asString
      val expected = CreateResponse(123)
      read[CreateResponse](result) must_== expected
    }

    "update a record" in {
      val result = Http(s"${server.url}record").option(HttpOptions.method("PUT")).asString
      val expected = CreateResponse(123)
      read[CreateResponse](result) must_== expected
    }
  }

  "File server" should {
    "provide script file" in {
      val result = Http.get(s"${server.url}js/t.js")
      result.responseCode must_== 200
    }
  }
}
