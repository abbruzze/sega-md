package ucesoft.smd.ui

import ucesoft.smd.cheat.{Cheat, CheatManager}
import ucesoft.smd.misc.Preferences

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.{BorderLayout, Dimension, FlowLayout}
import javax.swing.*
import javax.swing.event.{DocumentEvent, DocumentListener, ListSelectionListener}
import javax.swing.table.AbstractTableModel

class CheatPanel(frame:JFrame, manager:CheatManager, gameName:Option[String], pref:Preferences,cartInserted:Boolean) extends JPanel:
  private var applied = false
  private val sortedCheats = Cheat.gameCheats.sortBy(_.name)
  private class TableModel extends AbstractTableModel:
    private case class Row(game:String,codes:String,action:String)
    private var rows : Array[Row] = applyFilter(gameName.getOrElse(""))

    override def getColumnName(column: Int): String =
      column match
        case 0 => "Game name"
        case 1 => "Cheat codes"
        case 2 => "Action"

    override def getRowCount: Int = rows.length
    override def getColumnCount: Int = 3
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef =
      columnIndex match
        case 0 => rows(rowIndex).game
        case 1 => rows(rowIndex).codes
        case 2 => rows(rowIndex).action

    private def applyFilter(nameFilter:String): Array[Row] =
      val filter = nameFilter.toUpperCase()
      (for gc <- sortedCheats
           entry <- gc.cheats
           if gc.name.toUpperCase().contains(filter) yield
        Row(gc.name,entry.cheats.map(_.code).mkString(","),entry.description)
      ).toArray

    def setFilter(filter:String): Unit =
      rows = applyFilter(filter)
      fireTableDataChanged()

  init()

  val dialog : JDialog = {
    val dialog = new JDialog(frame,"Cheats",true)
    dialog.setLocationRelativeTo(frame)
    dialog.getContentPane.add("Center",this)
    dialog.pack()
    dialog.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit =
        dialog.dispose()
    })
    dialog
  }

  def isApplied : Boolean = applied

  private def init(): Unit =
    val applyButton = new JButton("Apply changes")
    applyButton.setEnabled(false)
    if !cartInserted then
      applyButton.setToolTipText("Cannot apply without a cartridge inserted")

    setLayout(new BoxLayout(this,BoxLayout.PAGE_AXIS))
    val filterPanel = new JPanel(new BorderLayout())
    val filterText = new JTextField(50)
    filterText.setText(gameName.getOrElse(""))
    var dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    dummyPanel.add(new JLabel("Name filter:",SwingConstants.RIGHT))
    dummyPanel.add(filterText)
    filterPanel.add("North",dummyPanel)
    val tableModel = new TableModel
    val filterTable = new JTable(tableModel)
    val tableSp = new JScrollPane(filterTable)
    filterPanel.add("Center",tableSp)
    filterPanel.setBorder(BorderFactory.createTitledBorder("Cheat Database"))

    filterText.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = tableModel.setFilter(filterText.getText)
      override def removeUpdate(e: DocumentEvent): Unit = tableModel.setFilter(filterText.getText)
      override def changedUpdate(e: DocumentEvent): Unit = tableModel.setFilter(filterText.getText)
    })

    val filterButtonPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    val addSelectedButton = new JButton("Add selected cheats")
    addSelectedButton.setEnabled(false)
    filterTable.getSelectionModel.addListSelectionListener(_ => addSelectedButton.setEnabled(filterTable.getSelectedRow > 0) )
    val cheatCodeText = new JTextField(8)
    val addCheatCodeButton = new JButton("Add cheat code")
    addCheatCodeButton.setEnabled(false)
    cheatCodeText.getDocument.addDocumentListener(new DocumentListener {
      override def insertUpdate(e: DocumentEvent): Unit = checkCheat()
      override def removeUpdate(e: DocumentEvent): Unit = checkCheat()
      override def changedUpdate(e: DocumentEvent): Unit = checkCheat()
      private def checkCheat(): Unit =
        Cheat.decode(cheatCodeText.getText.toUpperCase()) match
          case Some(Cheat.CheatCode(_, address, value)) =>
            cheatCodeText.setToolTipText(s"address = ${address.toHexString.toUpperCase()} value = ${value.toHexString.toUpperCase()}")
            addCheatCodeButton.setEnabled(true)
          case None =>
            cheatCodeText.setToolTipText("")
            addCheatCodeButton.setEnabled(false)
    })
    filterButtonPanel.add(addSelectedButton)
    filterButtonPanel.add(new JLabel("Cheat code:",SwingConstants.RIGHT))
    filterButtonPanel.add(cheatCodeText)
    filterButtonPanel.add(addCheatCodeButton)
    filterPanel.add("South",filterButtonPanel)

    add(filterPanel)

    add(Box.createRigidArea(new Dimension(0,10)))
    // ========================================================================

    val listModel = new DefaultListModel[String]
    addCheatCodeButton.addActionListener(_ => {
      if !listModel.contains(cheatCodeText.getText.toUpperCase()) then
        listModel.addElement(cheatCodeText.getText.toUpperCase())
        applyButton.setEnabled(cartInserted)
    })
    addSelectedButton.addActionListener(_ => {
      for r <- filterTable.getSelectedRows do
        val codes = tableModel.getValueAt(r,1).toString.toUpperCase().split(",")
        for code <- codes do
          if !listModel.contains(code) then
            listModel.addElement(code)
            applyButton.setEnabled(cartInserted)
    })
    for c <- manager.getCheats do
      listModel.addElement(c.code)

    val cheatList = new JList(listModel)
    cheatList.setPreferredSize(new Dimension(200,0))

    val cheatsPanel = new JPanel(new BorderLayout())
    cheatsPanel.setBorder(BorderFactory.createTitledBorder("Cheats"))
    val listSp = new JScrollPane(cheatList)
    dummyPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
    dummyPanel.add(listSp)
    val removeButton = new JButton("Remove selected")
    removeButton.setEnabled(false)
    removeButton.addActionListener(_ => {
      import scala.jdk.CollectionConverters.*
      for r <- cheatList.getSelectedValuesList.asScala do
        listModel.removeElement(r)
        applyButton.setEnabled(cartInserted)
    })
    dummyPanel.add(removeButton)
    cheatList.addListSelectionListener(_ => removeButton.setEnabled(cheatList.getSelectionModel.getSelectedItemsCount > 0))
    cheatsPanel.add("West",dummyPanel)

    add(cheatsPanel)
    // ========================================================================
    dummyPanel = new JPanel(new FlowLayout())
    dummyPanel.add(applyButton)
    applyButton.addActionListener(_ => {
      applyButton.setEnabled(false)
      manager.removeAllCheats()
      applied = true
      val sb = new StringBuilder
      for r <- 0 until listModel.getSize do
        Cheat.decode(listModel.elementAt(r)) match
          case Some(cheat) =>
            manager.addCheat(cheat)
            if r > 0 then sb.append('+')
            sb.append(cheat.code)
          case None =>
      pref.updateWithoutNotify(Preferences.CHEAT_CODES,sb.toString)
    })

    add(dummyPanel)