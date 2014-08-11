package ca.hyperreal.calab

import java.awt.{Dimension, Toolkit, BorderLayout, Graphics, FlowLayout, Graphics2D, Font}
import java.awt.Color._
import java.awt.event._
import javax.swing._
import javax.swing.SwingUtilities._
import javax.swing.event._
import java.util.concurrent.{ScheduledThreadPoolExecutor, TimeUnit, ScheduledFuture}

import util.Random._


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
	
	def mainFrame =
		new JFrame( "CALab Version 0.1" )
		{
		val screenSize = Toolkit.getDefaultToolkit.getScreenSize
		val inset = 30
		
			setBounds( inset, inset,
				screenSize.width  - inset*2,
				screenSize.height - inset*2 )
			createFrame
			setContentPane( desktop )
			addWindowListener(
				new WindowAdapter
				{
					override def windowClosing( e: WindowEvent )
					{
						threadPool.shutdown
						sys.exit( 0 )
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
									new AbstractAction( "New" )
									{
										def actionPerformed( e: ActionEvent )
										{
											createFrame
										}
									} ) )
						} )
				} )
		}
		
	def createFrame
	{
		desktop.add(
			new JInternalFrame
			{
			val gui = new RectangularGridGUI( this )
			
				setContentPane( gui )
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
							gui.stop
						}
					} )
			} )
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

	class RectangularGridGUI( f: JInternalFrame ) extends JPanel( new BorderLayout, true )
	{
		var gridWidth = 100
		var gridHeight = 100
		var planes = 100
		var pointSize = 5
		var spacing = 1
		var colors = Array( DARK_GRAY.darker.darker, WHITE )
		var period = 50
		var timer: ScheduledFuture[_] = null
		var engine = new LifeEngine( Set(3), Set(2, 3) )
		var threads = 4
		
		RectangularUniverse.init( 0 )
		
		val u = RectangularUniverse
		
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
											u.current(x)(y) = if (nextDouble < prob) 1 else 0
									}
									
									GridPanel.repaint()
								}
							} )
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
							RectangularUniverse.init( 0 )
						}
					} )
				add(
					number( gridHeight, "height" )
					{ n =>
						if (timer == null && n > 1 && n <= 500)
						{
							gridHeight = n
							GridPanel.updateSettings
							RectangularUniverse.init( 0 )
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
								RectangularUniverse.init( 0 )
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
					
		def stop
		{
			if (timer ne null)
			{
				timer.cancel( false )
				timer = null
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
				f.pack
				f.repaint()
			}
			
			def event2pos( e: MouseEvent ) = (e.getX/(pointSize + spacing), e.getY/(pointSize + spacing))
			
		var px: Int = _
		var py: Int = _
			
			def flip( x: Int, y: Int ) = RectangularUniverse.synchronized
			{
				px = x
				py = y
				u.current(x)(y) = (u.read( x, y ) + 1)%2
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
				
//			val g2d = g.asInstanceOf[Graphics2D]
				
			val cur = u.current
			
				for (x <- 0 until gridWidth; y <- 0 until gridHeight)
				{
				val x1 = x*(pointSize + spacing)
				val y1 = y*(pointSize + spacing)
				
					g setColor colors(cur(x)(y))
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
				
			init( 0 )
			
			def init( fill: Int )
			{
				array = new Array[Array[Array[Int]]]( planes )
				
				for (i <- 0 until planes)
					array( i ) = Array.fill( gridWidth, gridHeight )( fill )

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
	}
}

trait Universe
{
	def read( x: Int, y: Int ): Int
			
	def write( x: Int, y: Int, v: Int ): Unit
}

trait CAEngine extends ((Int, Int, Universe) => Unit)

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
		
		if (u.read( x, y ) == 0)
			u.write( x, y, if (birth( neighbours )) 1 else 0 )
		else
			u.write( x, y, if (survival( neighbours )) 1 else 0 )
	}
}