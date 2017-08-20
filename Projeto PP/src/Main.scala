import akka.actor._
import swing._
import swing.event._
import scala.concurrent

case class MapMe(t: String, l:List[String], n:Int)

class UIGreet extends MainFrame { //UI de boas vindas, faz nada, só pula pro UI seguinte
  title = "Bem vindo!"
  preferredSize = new Dimension(320, 240)
  contents = Button("Bem vindo!") { 
    this.close()
    val ui = new UIMain
    ui.visible = true
  }  
}

class UIMain extends MainFrame { //Nesse UI o usuário coloca o texto para procurar as palavras
  val SearchWords = new TextField{text = ""}
  var nPalavras = new TextField{text = "0"}
  val TextoMain = new TextArea{rows = 50; lineWrap = true; wordWrap = true}
  val buttonSearch = new Button("Relizar busca")
  title = "Palavras a buscar"
  contents = new BoxPanel(Orientation.Vertical){
    contents += new Label("Qual texto você fará a busca de palavras?")
    contents += Swing.VStrut(20)
    /*contents += new Label("Buscar por palavras específicas (Separar palavras por ';'): ")
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
    contents += Swing.VStrut(10)*/
    contents += new Label("Texto a se buscar palavras:")
    contents += new ScrollPane(TextoMain)
    contents += Swing.VStrut(20)
    contents += buttonSearch
    for(e<-contents) //empurra tudo para a esquerda
      e.xLayoutAlignment = 0
    border = Swing.EmptyBorder(15,15,15,15)
  }
  
  
  listenTo(buttonSearch) //Para evitar que o programa use o click sem ter de fato clicado, foi utilizado o reactions, que só se ativa
  reactions+={           //quando algo ocorre com o botão (ele é clicado)
    case ButtonClicked(b) => click()
  }
  def click(){ //Quando clicado, ele fecha a janela atual e começa a procurar as palavras usando o create Map no main.
    this.close()
    Main.createMap(MapMe(TextoMain.text, if (SearchWords.text.isEmpty()) List() else SearchWords.text.split(";").toList, nPalavras.text.toInt))
  }
}


class UIDone(m:Map[String,Integer]) extends MainFrame { //Após buscar os mapas, a UIDone é a última do programa (se possível,
  var labelWord = fillLabelsString(m) toArray           //implementar um loop entre UIDone e UIMain)
  var labelTimes = fillLabelsInteger(m) toArray //Listas baseadas no mapa adquirido
  var ArraysToTable = Array(labelWord, for( e <- labelTimes) yield e.toString)
  var tableWords = new Table(labelWord.length, 2)
  for(e <- 0 to labelWord.length - 1) {
    tableWords(e,0) = labelWord(e)
    tableWords(e,1) = labelTimes(e)
  }
  title = "Palavras encontradas"
  contents = new BoxPanel(Orientation.Vertical){
    contents += new ScrollPane(tableWords)
    contents += Swing.VStrut(30)
    contents += new BoxPanel(Orientation.Horizontal){
      contents += new Label("Total de palavras:") //Usando a função TotalWords, a quantidade de palavra total
      contents += Swing.HStrut(10)
      contents += new Label{
        text = TotalWords(fillLabelsInteger(m)).toString
      }
    }
    contents += Swing.VStrut(15)
    contents += new BoxPanel(Orientation.Horizontal){
      contents += new Label("Total de palavras diferentes:") //Usando a função TotalWords, a quantidade de palavra total
      contents += Swing.HStrut(10)
      contents += new Label{
        text = (fillLabelsInteger(m).length).toString
      }
    }
    contents += Swing.VStrut(15)
    contents += new BoxPanel(Orientation.Horizontal){
      contents += new Label("Palavra com maior frequência:") //Usando a função MostCommon, a palavra mais frequente aparece aqui
      contents += Swing.HStrut(10)
      contents += new Label{
        text = valueM((fillLabelsString(m)).lift(fillLabelsInteger(m).indexOf(MostCommon(fillLabelsInteger(m))))) +
        "    " + MostCommon(fillLabelsInteger(m))
      }
    }
    contents += Swing.VStrut(15)
    contents += new BoxPanel(Orientation.Horizontal){
      contents += new Label("Palavra com mais letras:") //Usando a função Largest, a maior palavra aparece aqui
      contents += Swing.HStrut(10)
      contents += new Label{
        text = valueM(Map(fillLabelsString(m) map { s => (s.length,s) } : _*).get(Largest(fillLabelsString(m)))) +
        "    " + Largest(fillLabelsString(m))
      }
    }
    for(e<-contents)
      e.xLayoutAlignment = 0
    border = Swing.EmptyBorder(15,15,15,15)
  }
  def Statistic(l:List[Integer], stat: (Integer,Integer) => Boolean): Integer = { //Função de alta ordem utilizada para realizar funções
    def go(l:List[Integer], i:Integer):Integer = {
      if(l.isEmpty) i
      else if(stat(l.head,i)) go(l.tail,l.head)
      else go(l.tail,i)
    }
    go(l,0)
  }
  def MostCommon(l:List[Integer]):Integer = { //Função MostCommon, aplicada em Statistic
    Statistic(l,_ > _)
  }
  def Largest(l:List[String]):Integer = { //Função Largest, aplicada em Statistic
    Statistic( for(e <- l) yield e.size.asInstanceOf[java.lang.Integer], _ > _ )
  }
  def TotalWords(l:List[Integer]):Integer = {
    l.foldLeft(0)((a,b) => a + b)
  }
  def fillLabelsInteger(m:Map[String,Integer]):List[Integer] = { //Função que retorna a parte Inteira do map
    def go(m:Map[String,Integer], s:List[Integer]):List[Integer] = {
      if (m.isEmpty) s
      else go(m.tail,s.::((m.head._2)))
    }
    go(m,List())
  }
  def fillLabelsString(m:Map[String,Integer]):List[String] = { //Função que retorna a parte String do map
    def go(m:Map[String,Integer], s:List[String]):List[String] = {
      if (m.isEmpty) s
      else go(m.tail, s.::(m.head._1))
    }
    go(m,List())
  }
  def valueM(x: Option[String]):String = x match{ //Função que retorna o Option
    case Some(s) => s
    case None => "None"
  }
}

object Main { //Objeto Main, onde ocorre a consolidação dos mapas
  
  case class Tarefa(text: String)
  case class Mapeamento(text:String)
  case class MapSize(size:Integer)
  case class MapReady(m:Map[String,Integer], b:Boolean)
  
  class Maping extends Actor { //Maping realiza as operações com o mapa em si
    var s:Integer = _
    def receive = {
      case MapSize(i) => s = i //Recebe a quantidade de Atores que estão ativos
      case Mapeamento(txt) => { //Ao receber um novo txt, reduz os atores ativos em 1
        if(s > 1) sender() ! MapReady(GenMap(txt), false) //caso ainda esteja com atores ativos, o MapReady n enviará o mapa
        else sender() ! MapReady(GenMap(txt), true) //caso n tenha mais atores, o MapReady enviará o mapa completo
        s-=1
      }
    }
    def GenMap(txt: String): Map[String, Integer] = { //Função que gera o mapa
      def go(l : List[String], m: Map[String, Integer]): Map[String, Integer] = { 
        if (l.isEmpty) m //Utilizando recursividade, o mapa é composto pela quebra do texto em letras e números (sem espaços ou pontos)
        else if (m.contains(l.head)) go(l.tail, m - l.head + (l.head->(m(l.head)+1))) //Caso o item já esteja no mapa, add 1
        else go(l.tail,m+(l.head->1)) //Se não, add o item com valor 1
      }
      go(txt.split("\\W+").filterNot(_ == "").toList,Map())
    }
  }
  
  class Cliente(servidor: ActorRef) extends Actor { //O cliente recebe o mapa e o consolida, além de enviar a
    var m:Map[String, Integer] = Map()              //quebra do texto para o gerador de mapa
    def receive = {
      case MapReady(mapR, false) => { //Enqto receber falso (ainda tem atores trabalhando), ele apenas consolida o mapa
        m = FindInMap(m,mapR)
      }
      case MapReady(mapR, true) => { //Qdo receber verdadeiro, ele consolida o mapa e o envia para o MapMerge
        //println(mapR)
        m = FindInMap(m,mapR)
        //println(m)
        //println()
        MapMerge(m)
      }
      case Tarefa(txt) => { //Ao receber o texto, ele faz a quebra em parágrafos (linhas novas) e as divide entre atores, 
        servidor ! MapSize(txt.split("\n").toList.size)//contando qtos atores serão criados também.
        txt.split("\n").toList.foreach(servidor ! Mapeamento(_))
      }
    }
    def FindInMap(m1:Map[String,Integer], m2:Map[String,Integer]):Map[String,Integer] = { //Função para consolidar dois mapas
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
  def createMap(e: Any) = e match { //FALTA IMPLEMENTAR CASO TENHA ESPECÍFICOS
    case MapMe(t,List(),0) => { //Caso não tenha palavras específicas, nem palavras mínimas, a quebra de texto ocorre normalmente
      val system = ActorSystem("System")
      val mapeador = system.actorOf(Props[Maping])
      val cliente =  system.actorOf(Props(classOf[Cliente], mapeador))
      cliente ! Tarefa(t)
    }
  }
  
  def MapMerge(m:Map[String,Integer]){ //Realiza update do mapa na variável sincronizada SyncM
    var SyncM = new SyncMap[Map[String,Integer]]
    if (!SyncM.isSet) SyncM.set(m)
    else if(SyncM.get.size <= m.size) SyncM.set(m)
    //println(SyncM.get)
    val ui = new UIDone(m) //Chama a UIDone para mostrar os resultados ao usuário
    ui.visible = true
  }
  
  class SyncMap[A]{ //Variável sincronizada
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
  
  def main(args: Array[String]) { //Main, abre o UIGreet
    val ui = new UIGreet
    ui.visible = true
  }
}
