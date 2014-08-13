package ca.hyperreal.calab

import java.awt.{Dimension, Toolkit, BorderLayout, Graphics, FlowLayout, Graphics2D, Font}
import java.awt.Color
import java.awt.Color._
import java.awt.event._
import javax.swing._
import javax.swing.SwingUtilities._
import javax.swing.event._
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit, ScheduledFuture}

import util.Random._

import ca.hyperreal.color.HSL


object Main extends App
{
	implicit def pair2dimension( pair: (Int, Int) ) = new Dimension( pair._1, pair._2 )
	
	lazy val desktop: JDesktopPane =
		new JDesktopPane
		{
			setBackground( DARK_GRAY )
		}
	val threadPool = new ScheduledThreadPoolExecutor( 20 )
	val iconFont = Font.createFont( Font.TRUETYPE_FONT, Main.getClass.getResourceAsStream("fontawesome-webfont.ttf") ).deriveFont( 10f )
	
	lazy val mainFrame =
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
							add(
								new JMenuItem(
									new AbstractAction( "New" )
									{
										def actionPerformed( e: ActionEvent )
										{
											desktop.add( new RectangularGridFrame )
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
						new JButton("Ok")
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

	class RectangularGridFrame extends JInternalFrame
	{
		var timer: ScheduledFuture[_] = null
		
		setContentPane(
			new JPanel( new BorderLayout, true )
			{
				var gridWidth = 100
				var gridHeight = 100
				var planes = 100
				var pointSize = 5
				var spacing = 1
				var period = 50
				var engine: CAEngine = new LifeEngine( Set(3), Set(2, 3) )
				var constructor: CAEngineConstructor = LifeEngine
				var threads = 4
				
				RectangularUniverse.init
				title
				
				add(
					new JPanel( new FlowLayout(FlowLayout.LEFT) )
					{
						add(
							new JComboBox( Array("1/8", "1/7", "1/6", "1/5", "1/4", "1/3", "0") )
							{
								addActionListener(
									new ActionListener
									{
										override def actionPerformed( e: ActionEvent )
										{
										val prob =
											Map(
												"1/8" -> 1.0/8, "1/7" -> 1.0/7, "1/6" -> 1.0/6, "1/5" -> 1.0/5,
												"1/4" -> 1.0/4, "1/3" -> 1.0/3, "0" -> 0.0 )( e.getSource.asInstanceOf[JComboBox[String]].getSelectedItem.asInstanceOf[String] )
												
											RectangularUniverse.synchronized
											{
												for (x <- 0 until gridWidth; y <- 0 until gridHeight)
													RectangularUniverse.current(x)(y) = if (nextDouble < prob) engine.alive else 0
											}
											
											GridPanel.repaint()
										}
									} )
							} )
						add(
							new JComboBox( Array(LifeEngine, GenEngine) )
							{
								addActionListener(
									new ActionListener
									{
										override def actionPerformed( e: ActionEvent )
										{
											constructor = e.getSource.asInstanceOf[JComboBox[CAEngineConstructor]].getSelectedItem.asInstanceOf[CAEngineConstructor]
										}
									} )
							} )
						add(
							new JTextField( "B3/S23", 5 ) with ActionListener
							{
								setBorder( BorderFactory.createTitledBorder("rule") )
								addActionListener( this )
								
								def actionPerformed( e: ActionEvent )
								{
									if (timer == null)
									{
										constructor( getText ) match
										{
											case None =>
											case Some( c ) =>
												engine = c
												title
												RectangularUniverse.init
												GridPanel.repaint()
										}
									}
								}
							} )
						add(
							icon( "\uf048" )
							{ b =>
								if (timer eq null)
								{
									RectangularUniverse.revert
									GridPanel.repaint()
								}
							} )
						add(
							icon( "\uf04b" )
							{ b =>
								if (timer eq null)
								{
									timer = animate
									b.setText( "\uf04d" )
								}
								else
								{
									stop
									b.setText( "\uf04b" )
								}
							} )
						add(
							icon( "\uf051" )
							{ b =>
								{
									if (timer eq null)
									{
										generation
										GridPanel.repaint()
									}
								}
							} )
						add(
							number( gridWidth, "width" )
							{ n =>
								if (timer == null && n > 1 && n <= 1000)
								{
									gridWidth = n
									GridPanel.updateSettings
									RectangularUniverse.init
								}
							} )
						add(
							number( gridHeight, "height" )
							{ n =>
								if (timer == null && n > 1 && n <= 500)
								{
									gridHeight = n
									GridPanel.updateSettings
									RectangularUniverse.init
								}
							} )
						add(
							number( pointSize, "size" )
							{ n =>
								if (n >= 1 && n < 500)
								{
									pointSize = n
									GridPanel.updateSettings
								}
							} )
						add(
							number( spacing, "space" )
							{ n =>
								if (n >= 0 && n < 500)
								{
									spacing = n
									GridPanel.updateSettings
								}
							} )
						add(
							number( period, "period" )
							{ n =>
								if (n >= 1 && n <= 2000)
								{
									period = n
									
									if (timer ne null)
									{
										stop
										timer = animate
									}
								}
							} )
						add(
							number( planes, "planes" )
							{ n =>
								if (n >= 2 && n <= 10000)
								{
									planes = n
									
									if (timer eq null)
									{
										RectangularUniverse.init
										GridPanel.repaint()
									}
								}
							} )
					}, BorderLayout.NORTH )
				add( GridPanel )
				GridPanel.settings
				
				def icon( name: String )( buttonAction: JButton => Unit ) =
					new JButton( name ) with ActionListener
					{
						setFont( iconFont )
						addActionListener( this )
					
						def actionPerformed( e: ActionEvent )
						{
							buttonAction( this )
						}
					}
				
				def number( init: Int, title: String )( fieldAction: Int => Unit ) =
					new JTextField( init.toString, 5 ) with ActionListener
					{
						setBorder( BorderFactory.createTitledBorder(title) )
						addActionListener( this )
						
						def actionPerformed( e: ActionEvent )
						{
							if (getText.matches("\\d+"))
								fieldAction( getText.toInt )
						}
					}
				
				def animate =
					threadPool.scheduleAtFixedRate(
						new Runnable
						{
							def run
							{
								generation
								GridPanel.repaint()
							}
						}, 0, period, TimeUnit.MILLISECONDS )

				def generation = RectangularUniverse.synchronized
				{
				val futures =
					for (r <- (0 until gridWidth).grouped(gridWidth/threads))
						yield
							threadPool.submit(
								new Runnable
								{
									def run
									{
										for (x <- r; y <- 0 until gridHeight)
											engine( x, y, RectangularUniverse )
									}
								} )
							
					for (f <- futures)
						f.get
						
					RectangularUniverse.tick
				}
				
				object GridPanel extends JPanel( true )
				{
					setBackground( BLACK )
					
					def settings
					{
						setPreferredSize( (gridWidth*(pointSize + spacing) - spacing, gridHeight*(pointSize + spacing) - spacing) )
					}
					
					def updateSettings
					{
						settings
						revalidate
						pack
					}
					
					def event2pos( e: MouseEvent ) = (e.getX/(pointSize + spacing), e.getY/(pointSize + spacing))
					
				var px: Int = _
				var py: Int = _
					
					def flip( x: Int, y: Int ) = RectangularUniverse.synchronized
					{
						px = x
						py = y
						RectangularUniverse.current(x)(y) =
							{
							val state = RectangularUniverse.read( x, y )
							
								if (state == 0)
									engine.alive
								else
									0
							}
						repaint()
					}
				
					addMouseListener(
						new MouseAdapter
						{
							override def mousePressed( e: MouseEvent )
							{
							val (x, y) = event2pos( e )
							
								flip( x, y )
							}
						} )
					
					addMouseMotionListener(
						new MouseAdapter
						{
							override def mouseDragged( e: MouseEvent )
							{
							val (x, y) = event2pos( e )
							
								if (x >= 0 && x < gridWidth && y >= 0 && y < gridHeight && (x != px || y != py))
									flip( x, y )
							}
						} )
					
					override def paintComponent( g: Graphics )
					{
						super.paintComponent( g )
						
					val cur = RectangularUniverse.current
					
						for (x <- 0 until gridWidth; y <- 0 until gridHeight)
						{
						val x1 = x*(pointSize + spacing)
						val y1 = y*(pointSize + spacing)
						
							g setColor engine.colors(cur(x)(y))
							g.fillRect( x1, y1, pointSize, pointSize )
						}
					}
				}
				
				object RectangularUniverse extends Universe
				{
					private var array: Array[Array[Array[Int]]] = _
					private var index: Int = _
					private var _current: Array[Array[Int]] = _
					private var _next: Array[Array[Int]] = _
					private var queue: Int = _
						
					init
					
					def init
					{
						array = new Array[Array[Array[Int]]]( planes )
						
						for (i <- 0 until planes)
							array( i ) = Array.fill( gridWidth, gridHeight )( 0 )

						_current = array( 0 )
						_next = array( 1 )
						index = 1
						queue = 0
					}
					
					def current = _current
					
					def next = _next
					
					def read( x: Int, y: Int ) = _current((x + gridWidth)%gridWidth)((y + gridHeight)%gridHeight)
					
					def write( x: Int, y: Int, v: Int ) = _next(x)(y) = v
					
					def tick
					{
						_current = _next
						index = (index + 1)%planes
						_next = array( index )
						queue = (queue + 1) min (planes - 1)
					}
					
					def back = (index - 1 + planes)%planes
					
					def revert
					{
						if (queue > 0)
						{
							_next = _current
							index = back
							_current = array( back )
							queue -= 1
						}
					}
				}
		
				def title
				{
					setTitle( engine.toString)
				}
			} )
			pack
			setIconifiable( true )
			setClosable( true )
			setVisible( true )
			setSelected( true )
			addInternalFrameListener(
				new InternalFrameAdapter
				{
					override def internalFrameClosed( e: InternalFrameEvent )
					{
						stop
					}
				} )
							
		def stop
		{
			if (timer ne null)
			{
				timer.cancel( false )
				timer = null
			}
		}
	}
}

trait Universe
{
	def read( x: Int, y: Int ): Int
			
	def write( x: Int, y: Int, v: Int ): Unit
}

trait CAEngineConstructor extends (String => Option[CAEngine])
{
	def string( s: String ) = s.map( _.toString.toInt ).toSet
}

trait CAEngine extends ((Int, Int, Universe) => Unit)
{
	def colors: Seq[Color]
	
	def alive: Int
}

object LifeEngine extends CAEngineConstructor
{
	val RULE = """B(\d*)/S(\d*)"""r
	
	def apply( rule: String ) =
	{
		if (RULE.pattern.matcher( rule ).matches)
		{
		val RULE(b, s) = rule
		
			Some( new LifeEngine(string(b), string(s)) )
		}
		else
			None
	}
	
	override def toString = "Life"
}

class LifeEngine( birth: Set[Int], survival: Set[Int] ) extends CAEngine
{
	def apply( x: Int, y: Int, u: Universe )
	{
	var neighbours = 0
	
		for (i <- -1 to 1)
		{
			neighbours += u.read( x + i, y - 1 )
			neighbours += u.read( x + i, y + 1 )
		}
		
		neighbours += u.read( x - 1, y )
		neighbours += u.read( x + 1, y )

		u.write( x, y, if ((if (u.read( x, y ) == 0) birth else survival)( neighbours )) 1 else 0 )
	}
	
	val colors = Seq( DARK_GRAY.darker.darker, WHITE )
	
	val alive = 1
	
	override def toString = s"""Life [birth: {${birth.toList.sorted.mkString(",")}}, survial: {${survival.toList.sorted.mkString(",")}}]"""
}

object GenEngine extends CAEngineConstructor
{
	val RULE = """(\d*)/(\d*)/(\d*)"""r
	
	def apply( rule: String ) =
	{
		if (RULE.pattern.matcher( rule ).matches)
		{
		val RULE(s, b, c) = rule
		
			Some( new GenEngine(string(b), string(s), c.toInt) )
		}
		else
			None
	}
	
	override def toString = "Gen"
}

class GenEngine( birth: Set[Int], survival: Set[Int], count: Int ) extends CAEngine
{
	val alive = count - 1
	
	def apply( x: Int, y: Int, u: Universe )
	{
		def living( x: Int, y: Int ) = if (u.read( x, y ) == alive) 1 else 0
		
	val state = u.read( x, y )
	
		if (state > 0 && state < alive)
			u.write( x, y, state - 1 )
		else
		{
		var neighbours = 0
		
			for (i <- -1 to 1)
			{
				neighbours += living( x + i, y - 1 )
				neighbours += living( x + i, y + 1 )
			}
			
			neighbours += living( x - 1, y )
			neighbours += living( x + 1, y )
			
			if (state == 0)
				u.write( x, y, if (birth( neighbours )) alive else 0 )
			else
				u.write( x, y, if (survival( neighbours )) alive else alive - 1 )
		}
	}
	
	val colors = Seq( DARK_GRAY.darker.darker ) ++ HSL( .55, 1, 0 ).shading( count - 2, .2 ) :+ WHITE
	
	override def toString = s"""Generations [birth: {${birth.toList.sorted.mkString(",")}}, survial: {${survival.toList.sorted.mkString(",")}}, count: $count]"""
}
