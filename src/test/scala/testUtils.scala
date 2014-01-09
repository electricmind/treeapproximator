package test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import ru.wordmetrix.utils.Utils._
import java.net.URI

class testUtils extends FlatSpec with Matchers {
    implicit def stringToURI(s: String) = new URI(s)
    implicit def uriToString(uri : String) = uri.toString()

    "A symbols" should "be escaped" in {
        uriToFilename("http://www.a-u.ru") should be("www.a--u.ru")
        uriToFilename("http://www.a:u.ru") should be("www.a__u.ru")
        uriToFilename("http://www.a_u.ru") should be("www.a___u.ru")
        uriToFilename("http://www.au.ru/oh") should be("www.au.ru---oh")
        uriToFilename("http://www.a--u.ru") should be("www.a----u.ru")
        uriToFilename("http://www.a:_u.ru") should be("www.a_____u.ru")
        uriToFilename("http://www.a_:u.ru") should be("www.a_____u.ru")
        uriToFilename("http://www.au.ru//oh") should be("www.au.ru------oh")
    }
    
    "A symbols" should "be restored" in {
        filenameToUri("www.a--u.ru").toString should be("http://www.a-u.ru")
        filenameToUri("www.a__u.ru").toString should be("http://www.a:u.ru")
        filenameToUri("www.a___u.ru").toString should be("http://www.a_u.ru")
        filenameToUri("www.au.ru---oh").toString should be("http://www.au.ru/oh")
        //filenameToUri("www.a----u.ru").toString should be("http://www.a--u.ru")
        //filenameToUri("www.a_____u.ru").toString should be("http://www.a:_u.ru")
        filenameToUri("www.a_____u.ru").toString should be("http://www.a_:u.ru")
        filenameToUri("www.au.ru------oh").toString should be("http://www.au.ru//oh")
    }
}