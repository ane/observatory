package observatory

import com.typesafe.scalalogging.LazyLogging

object Observatory extends App with LazyLogging {
  override def main(args: Array[String]): Unit = {
    logger.info("Starting Observatory...")
  }
}
