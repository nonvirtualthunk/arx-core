package arx.core.graphics.data

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 6/15/12
 * Time: 8:53 AM
 * Created by nonvirtualthunk
 */

import java.util.concurrent.Executors
import arx.core.{ResourceManager, Application, Moddable}

trait TToImage {
	def image : Image
}
class WrappedImage(val img:Image) extends TToImage { def image = img }
class FetchImage(val str:String) extends TToImage { def image = ResourceManager.getImage(str) }
class ImageFunc(func: => Image) extends TToImage {
	lazy val img = func
	def image = img
}

class LazyImageFunc(func: => Image) extends TToImage with Moddable[Image] {
	var img : Image = null
	def image = {
		if ( img eq null ) {
			img = LazyImageFunc.loadingImage
			LazyImageFunc.load(func,this)
			img
		} else { img }
	}

	def resolve() = image
	def baseValue() = image
	override def dynamic = true
}

class AsyncImage(func : => Image) extends LazyImageFunc(func) {
	private val tmp = image
}

object LazyImageFunc {
	val loadingImage = Image.Sentinel
	val threadpool = Executors.newCachedThreadPool()

	def load (func : => Image,holder : LazyImageFunc) {
		threadpool.submit(new ImageLoaderRunnable(func,holder))//execute(new ImageLoaderRunnable(func,holder))
	}

	class ImageLoaderRunnable(func : => Image,holder : LazyImageFunc) extends Runnable {
		def run() {
			val tmp : Image = func
			holder.img = tmp
		}
	}

	def shutDownThreadPool () {
		threadpool.shutdownNow()
	}
	Application.onQuit( shutDownThreadPool )
}
object TToImage {
	implicit def wrap (img:Image) : TToImage = new WrappedImage(img)
	implicit def wrap (str:String) : TToImage = new FetchImage(str)
	implicit def unwrap (t : TToImage) : Image = t.image

	def apply(img:Image) : TToImage = new WrappedImage(img)
	def apply (str:String) : TToImage = new FetchImage(str)
}