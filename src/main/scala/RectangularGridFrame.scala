package ca.hyperreal.calab

import java.awt.{Cursor, Dimension, Toolkit, BorderLayout, Graphics, FlowLayout, Graphics2D, Font, Color}
import Color._
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
import collection.mutable.{Buffer}


class RectangularGridFrame( settings: Map[Symbol, Any] = Map() ) extends JInternalFrame
{
	var timer: ScheduledFuture[_] = null
	val iconFont = Font.createFont( Font.TRUETYPE_FONT, Main.getClass.getResourceAsStream("fontawesome-webfont.ttf") ).deriveFont( 10f )

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
			
			for ((k, v) <- settings)
				(k, v) match
				{
					case ('width, w: Int) => gridWidth = w
					case ('height, h: Int) => gridHeight = h
					case ('size, s: Int) => pointSize = s
					case ('spacing, s: Int) => spacing = s
					case ('period, p: Int) => period = p
				}
					
			RectangularUniverse.init
			title
			
			add(
				new JPanel( new FlowLayout(FlowLayout.LEFT) )
				{
					add(
						new JComboBox( Array("any", "1/8", "1/7", "1/6", "1/5", "1/4", "1/3", "0") )
						{
							addActionListener(
								new ActionListener
								{
									override def actionPerformed( e: ActionEvent )
									{
									val r =
										{
										val s = e.getSource.asInstanceOf[JComboBox[String]].getSelectedItem.asInstanceOf[String]
										
											if (s == "any")
												() => nextInt( engine.maxValue + 1 )
											else
											{
											val prob =
												Map(
													"1/8" -> 1.0/8, "1/7" -> 1.0/7, "1/6" -> 1.0/6, "1/5" -> 1.0/5,
													"1/4" -> 1.0/4, "1/3" -> 1.0/3, "0" -> 0.0 )( s )
													
												() => if (nextDouble < prob) engine.maxValue else 0
											}
										}
												
										RectangularUniverse.synchronized
										{
											for (x <- 0 until gridWidth; y <- 0 until gridHeight)
												RectangularUniverse.current(x)(y) = r()
										}
										
										GridPanel.repaint()
									}
								} )
						} )
					add(
						new JComboBox( engines.toArray )
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
						{ _ =>
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
						{ _ =>
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
							if (timer == null && n > 1 && n <= 2000)
							{
								gridWidth = n
								GridPanel.updateSettings
								RectangularUniverse.init
							}
						} )
					add(
						number( gridHeight, "height" )
						{ n =>
							if (timer == null && n > 1 && n <= 1000)
							{
								gridHeight = n
								GridPanel.updateSettings
								RectangularUniverse.init
							}
						} )
					add(
						number( pointSize, "size" )
						{ n =>
							if (n >= 1 && n < 50)
							{
								pointSize = n
								GridPanel.updateSettings
							}
						} )
					add(
						number( spacing, "space" )
						{ n =>
							if (n >= 0 && n < 50)
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
			
			val animateRunnable =
				new Runnable
				{
					def run
					{
						generation
						GridPanel.repaint()
					}
				}
					
			def animate = threadPool.scheduleAtFixedRate( animateRunnable, 0, period, TimeUnit.MILLISECONDS )
					
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
								engine.maxValue
							else
								0
						}
					repaint()
				}
			
				var button1 = false
				var store = false
				var storex = 0
				var storey = 0
				var mx = 0
				var my = 0
				
				addMouseListener(
					new MouseAdapter
					{						
						override def mousePressed( e: MouseEvent )
						{
							e.getButton match
							{
								case MouseEvent.BUTTON1 =>
									val (x, y) = event2pos( e )
								
									if (x >= 0 && x < gridWidth && y >= 0 && y < gridHeight)
										if (store)
										{
											store = false
											GridPanel.setCursor( Cursor.getDefaultCursor )
											
										val ux = storex min x
										val uy = storey min y
										val lx = storex max x
										val ly = storey max y
										val w = (ux - lx).abs + 1
										val h = (uy - ly).abs + 1											
										val pat = new Pattern( w, h )
										
											for (i <- 0 until w; j <- 0 until h)
												pat.put( i, j, RectangularUniverse.current(ux + i)(uy + j) )
												
											showInputDialog( RectangularGridFrame.this, "Enter pattern name." ) match
											{
												case null | "" =>
												case name => patterns(name) = pat
											}
										}
										else
										{
											flip( x, y )
											button1 = true
										}
								case MouseEvent.BUTTON3 =>
									val (x, y) = event2pos( e )
								
									if (x >= 0 && x < gridWidth && y >= 0 && y < gridHeight)
									{
									val popupMenu =
										new JPopupMenu
										{
											add(
												new JMenu( "Insert pattern" )
												{
													for (k <- patterns.keys)
														add(
															buttonAction( k )
															{
																RectangularUniverse.put( x, y, patterns(k) )
																GridPanel.repaint()
															} )
												} )
											add(
												buttonAction( "Store pattern" )
												{
													GridPanel.setCursor( Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR) )
													store = true
													storex = x
													storey = y
													GridPanel.repaint()
												} )
										}
										
										popupMenu.show( GridPanel.this, e.getX, e.getY )
									}
							}
						}
						
						override def mouseReleased( e: MouseEvent )
						{
							if (e.getButton == MouseEvent.BUTTON1)
								button1 = false
						}
					} )
				
				addMouseMotionListener(
					new MouseMotionAdapter
					{
						override def mouseDragged( e: MouseEvent )
						{
							if (button1)
							{
							val (x, y) = event2pos( e )
							
								if (x >= 0 && x < gridWidth && y >= 0 && y < gridHeight && (x != px || y != py))
									flip( x, y )
							}
						}
						
						override def mouseMoved( e: MouseEvent )
						{
						val (x, y) = event2pos( e )
						
							if (x >= 0 && x < gridWidth)
								mx = x
								
							if (y >= 0 && y < gridHeight)
								my = y
							
							if (store)
								repaint()
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
					
					if (store)
					{
						g setColor DARK_GRAY
						
					val x = storex min mx
					val y = storey min my
					val w = (storex - mx).abs
					val h = (storey - my).abs
					
						g.drawRect( x*(pointSize + spacing), y*(pointSize + spacing), w*(pointSize + spacing) + pointSize - 1, h*(pointSize + spacing) + pointSize - 1 )
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
				
				def put( x: Int, y: Int, pat: Pattern ) = synchronized
				{
					for (i <- 0 until pat.width; j <- 0 until pat.height)
						_current((x + i)%gridWidth)((y + j)%gridHeight) = pat.get( i, j )
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
				setTitle( engine.toString )
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
