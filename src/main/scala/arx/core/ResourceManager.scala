package arx.core

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 4:23 PM
 * Created by nonvirtualthunk
 */

import graphics.data.Image

object ResourceManager {
	def getImage ( str : String ) = Image.withDimensions(100,100)
	def image ( str : String ) = getImage(str)
}