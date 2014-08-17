package ca.hyperreal

import java.awt.{Dimension}
import java.awt.event.{ActionEvent}
import javax.swing.{AbstractAction}
import java.util.concurrent.{ScheduledThreadPoolExecutor}

import collection.mutable.{HashMap, ArrayBuffer}


package object calab
{
	implicit def pair2dimension( pair: (Int, Int) ) = new Dimension( pair._1, pair._2 )
	
	def buttonAction( s: String )( thunk: => Unit ) =
		new AbstractAction( s )
		{
			def actionPerformed( e: ActionEvent ) = thunk
		}
	
	private [calab] val threadPool = new ScheduledThreadPoolExecutor( 20 )
	private [calab] val engines = new ArrayBuffer[CAEngineConstructor]
	private [calab] val patterns = new HashMap[String, Pattern]
}