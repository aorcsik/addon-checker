package com.aorcsik

import java.io.File
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
  val Addons = for (dir <- new File(addon_directory).listFiles if dir.isDirectory;
                    toc <- new File(dir.getPath).listFiles if toc.getName endsWith ".toc" ) yield {
    val source = scala.io.Source.fromFile(toc.getPath)
    val addon = source.getLines.foldLeft(("", "", "", "", "")) ((data, line) => data match { case (t, n, a, v, c) =>
      parseLine(line) match {
        case Some(("title", title)) => (title, n, a, v, c)
        case Some(("notes", notes)) => (t, notes, a, v, c)
        case Some(("author", author)) => (t, n, author, v, c)
        case Some(("version", version)) => (t, n, a, version, c)
        case Some(("curse_version", curse_version)) => (t, n, a, v, curse_version)
        case Some(_) => data
        case None => data
      }
    })
    source.close()
    addon
  }
}
