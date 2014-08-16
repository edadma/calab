package ca.hyperreal.calab

import java.awt.{Cursor, Dimension, Toolkit, BorderLayout, Graphics, FlowLayout, Graphics2D, Font}
import java.awt.Color._
import java.awt.event._
import javax.swing._
import SwingUtilities._
import JOptionPane._
import javax.swing.filechooser._
import javax.swing.event._
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit, ScheduledFuture}
import java.net.URL

import util.Random._
import io.Source
import collection.mutable.{HashMap, ArrayBuffer}


object Main extends App
{	
	lazy val desktop: JDesktopPane =
		new JDesktopPane
		{
			setBackground( DARK_GRAY )
		}
	val patternChooser =
		new JFileChooser
		{
			setFileFilter( new FileNameExtensionFilter( "Plaintext Patterns", "cells" ) )
		}
		
	def patternFromSource( defaultName: String, s: Source ): Option[(String, Pattern)] =
	{
	val buf = new ArrayBuffer[String]
	var name = defaultName
	var width = 0
	
		for (line <- s.getLines)
			if (line.startsWith( "!" ))
			{
				if (line.startsWith( "!Name:" ))
					name = line.substring( 6 ).trim
			}
			else
			{
			val l = line.trim
			
				if (!l.matches( "[.O]*"))
				{
					showMessageDialog( mainFrame,
										"pattern lines can only have .'s and O's", "Error loading pattern",  ERROR_MESSAGE )
					return None
				}
				
				width = width max l.length
				buf += l
			}
			
	val pat = new Pattern( width, buf.size )
	
		for (i <- 0 until buf.size; j <- 0 until buf(i).length)
			pat.put( j, i, if (buf(i)(j) == 'O') 1 else 0 )
			
		Some(name, pat)
	}
	
	lazy val mainFrame: JFrame =
		new JFrame( "CALab Version 0.1" )
		{
		val screenSize = Toolkit.getDefaultToolkit.getScreenSize
		val inset = 20
		
			setBounds( inset, inset,
				screenSize.width  - inset*8,
				screenSize.height - inset*2 )
			desktop.add( new RectangularGridFrame )
			setContentPane( desktop )
			addWindowListener(
				new WindowAdapter
				{
					override def windowClosing( e: WindowEvent )
					{
						quit
					}
				} )
			
			setJMenuBar(
				new JMenuBar
				{
					add(
						new JMenu( "File" )
						{
							add(
								new JMenuItem(
									new AbstractAction( "Load Engine" )
									{
										def actionPerformed( e: ActionEvent )
										{
											showInputDialog( mainFrame, "Enter fully qualified class name." ) match
											{
												case null =>
												case s if s.trim == "" =>
												case s =>
													try
													{
														engines += Class.forName( s ).newInstance.asInstanceOf[CAEngineConstructor]
													}
													catch
													{
														case e => showMessageDialog( mainFrame, "problem loading engine" )
													}
											}
										}
									} ) )
							add(
								new JMenuItem(
									new AbstractAction( "Load Pattern from File" )
									{
										def actionPerformed( e: ActionEvent )
										{
											if (patternChooser.showOpenDialog( mainFrame ) == JFileChooser.APPROVE_OPTION)
											{
												patternFromSource( patternChooser.getSelectedFile.getName, 
													io.Source.fromFile(patternChooser.getSelectedFile) ) match
												{
													case Some( kv ) => patterns += kv
													case None =>
												}
											}
										}
									} ) )
							add(
								new JMenuItem(
									new AbstractAction( "Load Pattern from URL" )
									{
										def actionPerformed( e: ActionEvent )
										{
											showInputDialog( mainFrame, "Enter URL of pattern." ) match
											{
												case null | "" =>
												case u => 
													val url = new URL( u )
													
													patternFromSource( url.getFile, io.Source.fromURL(url) ) match
													{
														case Some( kv ) => patterns += kv
														case None =>
													}
											}
										}
									} ) )
							add(
								new JMenuItem(
									new AbstractAction( "Quit" )
									{
										def actionPerformed( e: ActionEvent )
										{
											quit
										}
									} ) )
						} )
					add(
						new JMenu( "Grid" )
						{
						val by = "\u00d7"
						
							add(
								new JMenuItem(
									new AbstractAction( s"100${by}100, 20 frames per second" )
									{
										def actionPerformed( e: ActionEvent )
										{
										val grid = new RectangularGridFrame
										
											desktop.add( grid )
											grid.toFront
										}
									} ) )
							add(
								new JMenuItem(
									new AbstractAction( s"20${by}20, large squares, half second period" )
									{
										def actionPerformed( e: ActionEvent )
										{
										val grid = new RectangularGridFrame( Map('width -> 20, 'height -> 20, 'size -> 20, 'spacing -> 2, 'period -> 500) )
										
											desktop.add( grid )
											grid.toFront
										}
									} ) )
							add(
								new JMenuItem(
									new AbstractAction( s"500${by}500, pixel squares, fasted possible" )
									{
										def actionPerformed( e: ActionEvent )
										{
										val grid = new RectangularGridFrame( Map('width -> 500, 'height -> 500, 'size -> 1, 'spacing -> 0, 'period -> 1) )
										
											desktop.add( grid )
											grid.toFront
										}
									} ) )
							add(
								new JMenuItem(
									new AbstractAction( s"1200${by}600, pixel squares, fasted possible" )
									{
										def actionPerformed( e: ActionEvent )
										{
										val grid = new RectangularGridFrame( Map('width -> 1200, 'height -> 600, 'size -> 1, 'spacing -> 0, 'period -> 1) )
										
											desktop.add( grid )
											grid.toFront
										}
									} ) )
						} )
					add(
						new JMenu( "Help" )
						{
							add(
								new JMenuItem(
									new AbstractAction( "About" )
									{
										def actionPerformed( e: ActionEvent )
										{
											aboutFrame.setVisible( true )
										}
									} ) )
						} )
				} )
		}
	lazy val aboutFrame: JDialog =
		new JDialog( mainFrame, "About DialogTest", true )
		{
			add(
				new JLabel(
					"""<html>
						<h1><i>CALab Version 0.1</i></h1>
						<hr>(c) 2014 Edward A. Maxedon, Sr.
					</html>""" ), BorderLayout.CENTER )
			add(
				new JPanel
				{
					add(
						new JButton( "Ok" )
						{
							addActionListener(
								new ActionListener
								{
									def actionPerformed( e: ActionEvent )
									{
										aboutFrame.setVisible( false )
									}
								} )
						} )
				}, BorderLayout.SOUTH )
			setSize( 250, 150 )
			setLocationRelativeTo( mainFrame )
		}
	
	def quit
	{
		threadPool.shutdown
		sys.exit( 0 )
	}

	invokeAndWait(
		new Runnable
		{
			def run
			{
				JFrame.setDefaultLookAndFeelDecorated( true )
				mainFrame.setVisible( true )
			}
		} )
}

trait Universe
{
	def read( x: Int, y: Int ): Int
			
	def write( x: Int, y: Int, v: Int ): Unit
}