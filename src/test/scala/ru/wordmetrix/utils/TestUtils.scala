package ru.wordmetrix.utils

import java.net.URI

import org.scalatest.{FlatSpec, Matchers, WordSpecLike}

import ru.wordmetrix.utils.impl.{StirngEx, URIEx}

class TestUtils extends WordSpecLike with Matchers {
    implicit def stringToURI(s: String) = new URI(s)
    implicit def uriToString(uri: String) = uri.toString()
    implicit class StringEx(uri: String) {
        def toFilename = new URI(uri).toFilename
    }

    "A symbols" should {
        "be escaped" in {
            "http://www.a-u.ru".toFilename should be("www.a--u.ru")
            "http://www.a:u.ru".toFilename should be("www.a__u.ru")
            "http://www.a_u.ru".toFilename should be("www.a___u.ru")
            "http://www.au.ru/oh".toFilename should be("www.au.ru---oh")
            "http://www.a--u.ru".toFilename should be("www.a----u.ru")
            "http://www.a:_u.ru".toFilename should be("www.a_____u.ru")
            "http://www.a_:u.ru".toFilename should be("www.a_____u.ru")
            "http://www.au.ru//oh".toFilename should be("www.au.ru------oh")
        }

        "be restored" in {
            "www.a--u.ru".toURI.toString should be("http://www.a-u.ru")
            "www.a__u.ru".toURI.toString should be("http://www.a:u.ru")
            "www.a___u.ru".toURI.toString should be("http://www.a_u.ru")
            "www.au.ru---oh".toURI.toString should be("http://www.au.ru/oh")
            //"www.a----u.ru".toURI.toString should be("http://www.a--u.ru")
            //"www.a_____u.ru".toURI.toString should be("http://www.a:_u.ru")
            "www.a_____u.ru".toURI.toString should be("http://www.a_:u.ru")
            "www.au.ru------oh".toURI.toString should be("http://www.au.ru//oh")
        }
    }
}