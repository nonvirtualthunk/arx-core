package arx.core.graphics.data.attributeprofiles

import arx.core.graphics.data.AttributeProfile
import org.lwjgl.opengl.GL11

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 3/4/13
 * Time: 4:51 PM
 * Created by nonvirtualthunk
 */

object SimpleAttributeProfile extends AttributeProfile(List("vertex" -> (3,GL11.GL_FLOAT),"texCoord" -> (2,GL11.GL_FLOAT),"color" -> (4,GL11.GL_UNSIGNED_BYTE))) {
	val VertexAttribute = attributesByName("vertex")
	val TexCoordAttribute = attributesByName("texCoord")
	val ColorAttribute = attributesByName("color")

	vertexAttributeIndex = VertexAttribute
	texCoordAttributeIndex = TexCoordAttribute
}