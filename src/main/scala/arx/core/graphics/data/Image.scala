package arx.core.graphics.data

import java.nio.ByteBuffer
import java.io._
import arx.core.vec.{ReadVec4i, Vec4i}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import arx.core.datastructures.Rect
import scala.Some

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/30/11
 * Time: 4:13 PM
 * Created by nonvirtualthunk
 */

class Image{
	var hasAlpha: Boolean = true

	var width: Int = 0
	var height: Int = 0
	def aspectRatio = width.toFloat / math.max(height.toFloat,1.0f)
	def invAspectRatio = height.toFloat / math.max(width.toFloat,1.0f)
	var textureWidth: Int = 0
	var textureHeight: Int = 0
	var data: ByteBuffer = null
	var resourcePath : Option[String] = None
	var lastModified : Option[Long] = None
	var sentinel = false

	def fileName = resourcePath match {
		case None => "UnknownSource"
		case Some(rp) => rp.split("/").toList match {
			case Nil => "Root"
			case other => other.last
		}
	}

	def raw (x : Int,y : Int, rgba : Int) : Byte = data.get(y * textureWidth * 4 + x * 4 + rgba)

	def apply ( x: Int, y: Int ) : Vec4i = {
		data.position(y * textureWidth * 4 + x * 4)
		Vec4i(data.get() & 0xff,data.get() & 0xff,data.get() & 0xff,data.get() & 0xff)
	}
	def apply ( x : Int , y : Int , rgba: Int ) : Int = {
		data.get(y * textureWidth * 4 + x * 4 + rgba) & 0xff
	}
	def update ( x: Int, y: Int , v : Vec4i) {
		data.position(y * textureWidth * 4 + x * 4)
		data.put(v.r.toByte)
		data.put(v.g.toByte)
		data.put(v.b.toByte)
		data.put(v.a.toByte)
	}
	def pixelIndex ( x : Int , y : Int ) = y * textureWidth * 4 + x * 4
	def setAtIndex ( i : Int , rgba : Int , b : Byte )  { data.put(i + rgba,b) }
	def update ( x : Int , y : Int , rgba: Int , b: Byte ){
		data.put(y * textureWidth * 4 + x * 4 + rgba,b)
	}


	def downsample(factor: Int) : Image = {
		var target: Image = null
		var f = 0
		var src = this

		while ( f < factor ){
			target = Image.withDimensions(textureWidth >> factor,textureHeight >> factor)
			for ( x <- 0 until textureWidth by 2 ; y <- 0 until textureHeight by 2 ) {
				target(x,y) = (src(x,y) + src(x+1,y) + src(x,y+1) + src(x+1,y+1)) / 4
			}
			src = target
			f += 1
		}

		target
	}

	def setPixelsFromFunc ( func : (Int,Int,Int) => Int ) {
		var x = 0; while ( x < width ) {
			var y = 0; while ( y < height ) {
				var q = 0; while ( q < 4 ) {
					this(x,y,q) = func(x,y,q).toByte
				q += 1}
			y += 1}
		x += 1}
	}
	def setPixelsFromFunc ( func : (Int,Int) => ReadVec4i ) {
		var x = 0; while ( x < width ) {
			var y = 0; while ( y < height ) {
				this(x,y) = func(x,y)
			y += 1}
		x += 1}
	}
	def transformPixelsByFunc ( func : (Int,Int,ReadVec4i) => ReadVec4i ) {
		val holder = Vec4i(0,0,0,0)
		var x = 0; while ( x < width ) {
			var y = 0; while ( y < height ) {
				var q = 0; while ( q < 4 ) {
					holder(q) = this(x,y,q)
				q += 1}
				val newVal = func(x,y,holder)
				if ( ! ( newVal eq holder ) ) {
					this(x,y) = newVal
				}
			y += 1}
		x += 1}
	}
}

class SubImageView(subImage:Image,region:Rect[Int]) extends Image {
	hasAlpha = subImage.hasAlpha
	width = region.width
	height = region.height
	data = subImage.data
	resourcePath = subImage.resourcePath
	lastModified = subImage.lastModified

	textureWidth = subImage.textureWidth
	textureHeight = subImage.textureHeight

	override def raw (x : Int,y : Int, rgba : Int) : Byte = {
		data.get((y + region.y) * textureWidth * 4 + (x + region.x) * 4 + rgba)
	}

	override def apply ( x: Int, y: Int ) : Vec4i = {
		data.position((y+region.y) * textureWidth * 4 + (x+region.x) * 4)
		Vec4i(data.get() & 0xff,data.get() & 0xff,data.get() & 0xff,data.get() & 0xff)
	}
	override def apply ( x : Int , y : Int , rgba: Int ) : Int = {
		data.get((y+region.y) * textureWidth * 4 + (x+region.x) * 4 + rgba) & 0xff
	}
	override def update ( x: Int, y: Int , v : Vec4i) {
		data.position((y+region.y) * textureWidth * 4 + (x+region.x) * 4)
		data.put(v.r.toByte)
		data.put(v.g.toByte)
		data.put(v.b.toByte)
		data.put(v.a.toByte)
	}
	override def pixelIndex ( x : Int , y : Int ) = (y+region.y) * textureWidth * 4 + (x+region.x) * 4
	override def setAtIndex ( i : Int , rgba : Int , b : Byte )  {
		throw new UnsupportedOperationException("No set by index in image sub view")
	}
	override def update ( x : Int , y : Int , rgba: Int , b: Byte ){
		data.put((y+region.y) * textureWidth * 4 + (x+region.x) * 4 + rgba,b)
	}
}

object Image{
	def loadFromFile (filePath: String): Image = {
		val file = new File(filePath)
		val stream = new FileInputStream(file)
		val image = loadFromStream(stream,closeOnFinish = true)
		image.resourcePath = Some(filePath)
		image.lastModified = Some(file.lastModified)

		image
	}

	def loadFromStream (inputStream : InputStream,closeOnFinish : Boolean) : Image = {
		val data = new PNGImageData2
		val buffer = data.loadImage(inputStream,true,true,null)
		if ( closeOnFinish ) { inputStream.close() }

		val ret = new Image
		ret.data = buffer
		ret.width = data.getWidth
		ret.height = data.getHeight
		ret.hasAlpha = data.hasAlpha
		ret.textureWidth = data.getTexWidth
		ret.textureHeight = data.getTexHeight
		ret
	}

	def withDimensions(width: Int,height: Int): Image = {
		val img = new Image
		img.width = width
		img.height = height
		img.textureWidth = width
		img.textureHeight = height
		img.data = ByteBuffer.allocateDirect(width * height * 4)
		img.lastModified = Some(System.currentTimeMillis())
		img
	}

	def withDimensions(width: Int,height: Int,baseColor : Vec4i): Image = {
		val img = new Image
		img.width = width
		img.height = height
		img.textureWidth = width
		img.textureHeight = height
		img.data = ByteBuffer.allocateDirect(width * height * 4)
		img.lastModified = Some(System.currentTimeMillis())

		var x = 0; while ( x < img.width ) {
			var y = 0; while ( y < img.height ) {
				var q = 0; while ( q < 4 ) {
					img(x,y,q) = baseColor(q).toByte
				q += 1}
			y += 1}
		x += 1}

		img
	}

	def save( image : Image , path : String ) { save(image,new File(path)) }
	def save( image : Image , file : File ) {
        val fos = new FileOutputStream(file)
        save(image,fos)
        fos.close()
    }

    def save ( image : Image , stream : OutputStream ) {
		val icon = image
		val bi = new BufferedImage(icon.width,icon.height,BufferedImage.TYPE_INT_ARGB)

		val wr = bi.getData.createCompatibleWritableRaster()

		val packer = Array(0,0,0,0)
		for ( x <- 0 until icon.width ; y <- 0 until icon.height ) {
			val r = icon(x,y,Red) & 0xff
			val g = icon(x,y,Green) & 0xff
			val b = icon(x,y,Blue) & 0xff
			val a = icon(x,y,Alpha) & 0xff
			packer(0) = r
			packer(1) = g
			packer(2) = b
			packer(3) = a

			wr.setPixel(x,icon.height - y - 1,packer)
		}

		bi.setData(wr)
		ImageIO.write(bi,"png",stream)
	}

	def composite ( lowerImage : Image, upperImage : Image ) = {
		if ( lowerImage.width != upperImage.width || lowerImage.height != upperImage.height ) { println("Composite image was intended to be used with images of the same size") }
		val newImage = Image.withDimensions(lowerImage.width.max(upperImage.width),lowerImage.height.max(upperImage.height))
		lowerImage.data.rewind()
		newImage.data.put(lowerImage.data)
		lowerImage.data.rewind()
		newImage.data.rewind()

		var x = 0; while ( x < upperImage.width ) {
			var y = 0; while ( y < upperImage.height ) {
				if ( (upperImage.raw(x,y,Alpha) & 0xff) > 0 ) {
					newImage(x,y,Red) = upperImage.raw(x,y,Red)
					newImage(x,y,Green) = upperImage.raw(x,y,Green)
					newImage(x,y,Blue) = upperImage.raw(x,y,Blue)
					newImage(x,y,Alpha) = upperImage.raw(x,y,Alpha)
				}
			y += 1}
		x += 1}

		newImage
	}

	val Red = 0
	val Green = 1
	val Blue = 2
	val Alpha = 3

	val Sentinel = Image.withDimensions(1,1)
	Sentinel.sentinel = true
}

