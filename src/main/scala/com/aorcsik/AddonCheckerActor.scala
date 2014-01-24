package com.aorcsik

import akka.actor.Actor
import spray.routing._
import spray.http._
import MediaTypes._

/**
 * Created by aorcsik on 2014.01.24..
 */
class AddonCheckerActor extends Actor with AddonChecker {

  def actorRefFactory = context

  def receive = runRoute(myRoute)
}

trait AddonChecker extends HttpService {

  val myRoute =
    path("") {
      get {
        respondWithMediaType(`text/html`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            <html>
              <body>
                <h1>Say hello to <i>spray-routing</i> on <i>spray-can</i>!</h1>
              </body>
            </html>
          }
        }
      }
    }
}