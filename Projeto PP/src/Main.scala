import akka.actor._
import scala.util._
import swing._
import swing.event._
import scala.concurrent

case class MapMe(t: String, l:List[String], n:Int)

class UIGreet extends MainFrame {
  title = "Bem vindo!"
  preferredSize = new Dimension(320, 240)
  contents = Button("Bem vindo!") { 
    this.close()
    val ui = new UIMain
    ui.visible = true
  }  
}

class UIMain extends MainFrame {
  val SearchWords = new TextField{text = ""}
  var nPalavras = new TextField{text = "0"}
  val TextoMain = new TextArea{rows = 50; lineWrap = true; wordWrap = true}
  val buttonSearch = new Button("Relizar busca")
  title = "Palavras a buscar"
  contents = new BoxPanel(Orientation.Vertical){
    contents += new Label("Qual texto você fará a busca de palavras?")
    contents += Swing.VStrut(20)
    contents += new Label("Buscar por palavras específicas (Separar palavras por ';'): ")
    contents += Swing.VStrut(10)
    contents += SearchWords
    contents += Swing.VStrut(10)
    contents += Swing.Glue
      contents += new BoxPanel(Orientation.Horizontal){
        contents += new Label("Buscar por palavras que aparecem mais de:")
        contents += Swing.HStrut(10)
        contents += nPalavras
        contents += new Label(" vezes")
      }
    contents += Swing.VStrut(10)
    contents += new Label("Texto a se buscar palavras:")
    contents += TextoMain
    contents += Swing.VStrut(20)
    contents += buttonSearch
    for(e<-contents)
      e.xLayoutAlignment = 0
    border = Swing.EmptyBorder(15,15,15,15)
  }
  
  
  listenTo(buttonSearch)
  reactions+={
    case ButtonClicked(b) => click()
  }
  def click(){
    this.close()
    Main.createMap(MapMe(TextoMain.text, if (SearchWords.text.isEmpty()) List() else SearchWords.text.split(";").toList, nPalavras.text.toInt))
  }
}



class UIDone(m:Map[String,Integer]) extends MainFrame {
  var labelWord = new ListView(fillLabelsString(m))
  var labelTimes = new ListView(fillLabelsInteger(m))
  title = "Palavras encontradas"
  contents = new BoxPanel(Orientation.Vertical){
    contents += new BoxPanel(Orientation.Horizontal){
      contents += new BoxPanel(Orientation.Vertical){
        contents += new Label("Palavras encontradas:")
        contents += Swing.VStrut(10)
        contents += labelWord
      }
      contents += Swing.HStrut(50)
      contents += new BoxPanel(Orientation.Vertical){
        contents += new Label("Número de vezes encontradas:")
        contents += Swing.VStrut(10)
        contents += labelTimes
      }
    }
    contents += Swing.VStrut(30)
    contents += new Label("Palavra com maior frequência:")
    contents += Swing.HStrut(10)
    contents += new Label{
      text = valueM((fillLabelsString(m)).lift(fillLabelsInteger(m).indexOf(MostCommon(fillLabelsInteger(m))))) +
      "    " + MostCommon(fillLabelsInteger(m))
    }
    contents += Swing.VStrut(15)
    contents += new Label("Palavra com mais letras:")
    contents += Swing.HStrut(10)
    contents += new Label{
      text = valueM(Map(fillLabelsString(m) map { s => (s.length,s) } : _*).get(Largest(fillLabelsString(m)))) +
      "    " + Largest(fillLabelsString(m))
    } 
    for(e<-contents)
      e.xLayoutAlignment = 0
    border = Swing.EmptyBorder(15,15,15,15)
  }
  def Statistic(l:List[Integer], stat: (Integer,Integer) => Boolean): Integer = {
    def go(l:List[Integer], i:Integer):Integer = {
      if(l.isEmpty) i
      else if(stat(l.head,i)) go(l.tail,l.head)
      else go(l.tail,i)
    }
    go(l,0)
  }
  def MostCommon(l:List[Integer]):Integer = {
    Statistic(l,_ > _)
  }
  def Largest(l:List[String]):Integer = {
    Statistic( for(e <- l) yield e.size.asInstanceOf[java.lang.Integer], _ > _ )
  }
  def fillLabelsInteger(m:Map[String,Integer]):List[Integer] = {
    def go(m:Map[String,Integer], s:List[Integer]):List[Integer] = {
      if (m.isEmpty) s
      else go(m.tail,s.::((m.head._2)))
    }
    go(m,List())
  }
  def fillLabelsString(m:Map[String,Integer]):List[String] = {
    def go(m:Map[String,Integer], s:List[String]):List[String] = {
      if (m.isEmpty) s
      else go(m.tail, s.::(m.head._1))
    }
    go(m,List())
  }
  def valueM(x: Option[String]):String = x match{
    case Some(s) => s
    case None => "None"
  }
}

object Main {
  
  case class Tarefa(text: String)
  case class Mapeamento(text:String)
  case class MapSize(size:Integer)
  case class MapReady(m:Map[String,Integer], b:Boolean)
  
  class Maping extends Actor {
    var s:Integer = _
    def receive = {
      case MapSize(i) => s = i
      case Mapeamento(txt) => {
        if(s > 1) sender() ! MapReady(GenMap(txt), false)
        else sender() ! MapReady(GenMap(txt), true)
        s-=1
      }
    }
    def GenMap(txt: String): Map[String, Integer] = {
      def go(l : List[String], m: Map[String, Integer]): Map[String, Integer] = {
        if (l.isEmpty) m
        else if (m.contains(l.head)) go(l.tail, m - l.head + (l.head->(m(l.head)+1)))
        else go(l.tail,m+(l.head->1))
      }
      go(txt.split("\\W+").filterNot(_ == "").toList,Map())
    }
  }
  
  class Cliente(servidor: ActorRef) extends Actor {
    var m:Map[String, Integer] = Map()
    def receive = {
      case MapReady(mapR, false) => {
        m = FindInMap(m,mapR)
      }
      case MapReady(mapR, true) => {
        //println(mapR)
        m = FindInMap(m,mapR)
        //println(m)
        //println()
        MapMerge(m)
      }
      case Tarefa(txt) => {
        servidor ! MapSize(txt.split("\n").toList.size)
        txt.split("\n").toList.foreach(servidor ! Mapeamento(_))
      }
    }
    def FindInMap(m1:Map[String,Integer], m2:Map[String,Integer]):Map[String,Integer] = {
      def go(m1:Map[String,Integer], m2:Map[String,Integer], mM:Map[String,Integer]):Map[String,Integer] = {
        if (m1.isEmpty) m2 ++ mM
        else if (m2.isEmpty) m1 ++ mM
        else if(m1.get(m2.head._1) == None) go(m2.tail, m1, mM + (m2.head._1->m2.head._2))
        else go(m2.tail, m1.-(m2.head._1), mM + (m2.head._1->(m1(m2.head._1) + m2.head._2)))
      }
      go(m1,m2,Map())
    }
  }
  
  //def createMap(t: String, l:List[String], n:Integer) {
  def createMap(e: Any) = e match {
    case MapMe(t,List(),0) => {
      val system = ActorSystem("System")
      val mapeador = system.actorOf(Props[Maping])
      val cliente =  system.actorOf(Props(classOf[Cliente], mapeador))
      cliente ! Tarefa(t)
    }
  }
  
  def MapMerge(m:Map[String,Integer]){
    var SyncM = new SyncMap[Map[String,Integer]]
    if (!SyncM.isSet) SyncM.set(m)
    else if(SyncM.get.size <= m.size) SyncM.set(m)
    //println(SyncM.get)
    val ui = new UIDone(m)
    ui.visible = true
  }
  
  class SyncMap[A]{
    private var isDefined: Boolean = false
    private var value: A = _
    def get = synchronized {
      while(!isDefined) wait()
      value
    }
    def set(x:A) = synchronized {
      value = x; isDefined = true; notifyAll()
    }
    def isSet: Boolean = synchronized {
      isDefined
    }
    def unset = synchronized {
      isDefined = false
    }
  }
  
  def MapR(m:Map[String,Integer]) {
    var m:Map[String, Integer] = Map()
    
    def receiveMap(m1:Map[String,Integer], m2:Map[String,Integer]):Map[String,Integer] = {
      def go(m1:Map[String,Integer], m2:Map[String,Integer], mM:Map[String,Integer]):Map[String,Integer] = {
        if (m1.isEmpty) m2 ++ mM
        else if (m2.isEmpty) m1 ++ mM
        else if(m1.get(m2.head._1) == None) go(m2.tail, m1, mM + (m2.head._1->m2.head._2))
        else go(m2.tail, m1.-(m2.head._1), mM + (m2.head._1->(m1(m2.head._1) + m2.head._2)))
      }
      go(m1,m2,Map())
    }
  }
  
  def main(args: Array[String]) {
    val ui = new UIGreet
    ui.visible = true
  }
}