package com.aorcsik

import java.io.File
import scalaj.http._
import scala.collection.JavaConversions._

/**
 * Created by aorcsik on 2014.01.24..
 */
class AddonCatalog {

  val TitlePattern = "^## Title:\\s(.*)\\s*$".r
  val NotesPattern = "^## Notes:\\s(.*)\\s*$".r
  val AuthorPattern = "^## Author:\\s(.*)\\s*$".r
  val VersionPattern = "^## Version:\\s+(.*)\\s*$".r
  val CurseVersionPattern = "^## X-Curse-Packaged-Version:\\s+(.*)\\s*$".r

  def parseLine(line: String) = line match {
    case TitlePattern(title) => Some(("title", title))
    case NotesPattern(notes) => Some(("notes", notes))
    case AuthorPattern(author) => Some(("author", author))
    case VersionPattern(version) => Some(("version", version))
    case CurseVersionPattern(curse_version) => Some(("curse_version", curse_version))
    case _ => None
  }

  val addon_directory = "/Applications/World of Warcraft/Interface/AddOns"
  val addons = for (dir <- new File(addon_directory).listFiles if dir.isDirectory;
                    toc <- new File(dir.getPath).listFiles if toc.getName endsWith ".toc" ) yield {
    val time = dir.lastModified()
    val source = scala.io.Source.fromFile(toc.getPath)
    val addon = source.getLines.foldLeft(Addon("", time, "", "", "", "")) ((data, line) => data match { case Addon(t, time, n, a, v, c) =>
      parseLine(line) match {
        case Some(("title", title)) => Addon(title, time, n, a, v, c)
        case Some(("notes", notes)) => Addon(t, time, notes, a, v, c)
        case Some(("author", author)) => Addon(t, time, n, author, v, c)
        case Some(("version", version)) => Addon(t, time, n, a, version, c)
        case Some(("curse_version", curse_version)) => Addon(t, time, n, a, v, curse_version)
        case Some(_) => data
        case None => data
      }
    })
    source.close()
    addon
  }
}

case class Addon(override val title: String, override val last_mod: Long, notes: String, author: String, version: String, curse_version: String) extends AddonDiscovery

abstract class AddonDiscovery() {
  val title: String = ""
  val last_mod: Long = 0

  def search_term = title.replaceAll("[\\W_]+", " ").replaceAll("([a-z])([A-Z][a-z])", "$1 $2")

  def getWowInterfaceId = {
    print("Searching for %s (%d) on WoWInterface...".format(search_term, last_mod))
    val response = Http("http://www.wowinterface.com/downloads/search.php").params("search" -> search_term)
      .option(HttpOptions.connTimeout(1000)).option(HttpOptions.readTimeout(5000)).asString
    val P1 = "<a\\s.*?href=\"fileinfo.*?id=(\\d+)\".*?>\\W*%s".format(search_term.replaceAll(" ","\\W*")).r
    P1 findFirstIn response match {
      case Some(P1(id)) => Some(id.toInt)
      case _ => None
    }
  }

  def getCurseVersion = {
    print("Searching for %s (%d) on Curse...".format(search_term, last_mod))
    val response = Http("http://www.curse.com/search/addons").params("game-slug" -> "wow", "search" -> search_term)
      .option(HttpOptions.connTimeout(1000)).option(HttpOptions.readTimeout(5000)).asString
    response
  }
}

