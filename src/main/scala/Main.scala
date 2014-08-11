package ca.hyperreal.cal

import java.awt.{Dimension, Toolkit, BorderLayout, Graphics, FlowLayout, Graphics2D}
import java.awt.Color._
import java.awt.event._
import javax.swing._
import javax.swing.SwingUtilities._
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
	val threadPool = new ScheduledThreadPoolExecutor( 10 )
	
	def mainFrame =
		new JFrame
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
				}
			)
		}
		
	def createFrame
	{
	val frame = new JInternalFrame
	
		frame.setContentPane( new RectangularGridGUI )
		frame.pack
		frame.setIconifiable( true )
		frame.setClosable( true )
		frame.setVisible( true )
		desktop.add( frame )
		frame.setSelected( true )
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

	class RectangularGridGUI extends JPanel( new BorderLayout, true )
	{
		var gridWidth = 200
		var gridHeight = 120
		var planes = 2
		var pointSize = 4
		var spacing = 1
		var colors = Array( DARK_GRAY.darker.darker, WHITE )
		var rate = 20
		var timer: ScheduledFuture[_] = null
		var engine = new LifeEngine( Set(3), Set(2, 3) )
		var threads = 4
		
		RectangularUniverse.init( 0 )
		
		val u = RectangularUniverse
		
		add(
			new JPanel( new FlowLayout(FlowLayout.LEFT) )
			{
				add(
					new JButton(
						new AbstractAction( "New" )
						{
							def actionPerformed( e: ActionEvent )
							{
								if (timer eq null)
								{
									for (x <- 0 until gridWidth; y <- 0 until gridHeight)
										u.current(x)(y) = 0
								
									GridPanel.repaint()
								}
							}
						} ) )
				add(
					new JButton(
						new AbstractAction( "Random" )
						{
							def actionPerformed( e: ActionEvent )
							{
								RectangularUniverse.synchronized
								{
									for (x <- 0 until gridWidth; y <- 0 until gridHeight)
										u.current(x)(y) = nextInt( 8 )/7
								}
								
								GridPanel.repaint()
							}
						} ) )
				add(
					new JButton(
						new AbstractAction( "Animate" )
						{
							def actionPerformed( e: ActionEvent )
							{
								if (timer eq null)
									timer = animate
							}
						} ) )
				add(
					new JButton(
						new AbstractAction( "Pause" )
						{
							def actionPerformed( e: ActionEvent )
							{
								if (timer ne null)
								{
									timer.cancel( false )
									timer = null
								}
							}
						} ) )
				add(
					new JButton(
						new AbstractAction( "Forward" )
						{
							def actionPerformed( e: ActionEvent )
							{
								if (timer eq null)
								{
									generation
									GridPanel.repaint()
								}
							}
						} ) )
				add(
					new JButton(
						new AbstractAction( "Backward" )
						{
							def actionPerformed( e: ActionEvent )
							{
								if (timer eq null)
								{
									RectangularUniverse.revert
									GridPanel.repaint()
								}
							}
						} ) )
			}, BorderLayout.NORTH )
		add( GridPanel )
		GridPanel.settings
		
		def animate =
			threadPool.scheduleAtFixedRate(
				new Runnable
				{
					def run
					{
						generation
						GridPanel.repaint()
					}
				}, 0, rate, TimeUnit.MILLISECONDS )

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
				revalidate
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
			/*private*/ var index: Int = _
			private var _current: Array[Array[Int]] = _
			private var _next: Array[Array[Int]] = _
			
			init( 0 )
			
			def init( fill: Int )
			{
				array = new Array[Array[Array[Int]]]( planes )
				
				for (i <- 0 until planes)
					array( i ) = Array.fill( gridWidth, gridHeight )( fill )

				_current = array( 0 )
				_next = array( 1 )
				index = 1
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
			}
			
			def back = (index - 1 + planes)%planes
			
			def revert
			{
				_next = _current
				index = back
				_current = array( back )
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