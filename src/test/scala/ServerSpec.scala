package tsrv

import org.specs._

import scalaj.http.{HttpOptions, Http}

object ServerSpec extends Specification with unfiltered.spec.jetty.Served {
  def setup = { _.filter(new App) }

  "Client interface" should {
    "return a record" in {
      val result = Http.get(s"${server.url}record").asString
      result must_== """readResponse({"id":123,"teams":["A","B"]})"""
    }

    "store a record" in {
      val result = Http.post(s"${server.url}record").asString
      result must_== """{"id":123}"""
    }

    "update a record" in {
      val result = Http(s"${server.url}record").option(HttpOptions.method("PUT")).asString
      result must_== """{"id":123}"""
    }
  }
}
