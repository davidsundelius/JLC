import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Scanner;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;

public class JLDE extends JFrame {
  private static final long serialVersionUID = 1L;
  private File fileInUse = null;
  private File asmFileInUse = null;
  private File compiledFileInUse = null;
  private File compiler = null;
  
  private JTabbedPane tabs;
  
  private JLEditor rawCode;
  private JLEditor asmCode;
  private JTextPane commandLn;
  private JScrollPane commandLnScroll;
  
  private JButton btnOpen;
  private JButton btnReload;
  private JButton btnSave;
  private JButton btnCompile;
  private JButton btnAssemble;
  private JButton btnExecute;
  private JComboBox<String> cmbTarget;
  
  private JLabel lblStatus;
  
  private String commandlnLog = "";
  
  private JLDE() {
    setupWindow();
  }
  
  private void setupWindow() {
    //Add components
    tabs = new JTabbedPane();
    
    rawCode = new JLEditor();
    tabs.addTab("Javalette", rawCode);
    tabs.setMnemonicAt(0, KeyEvent.VK_1);
    
    asmCode = new JLEditor();
    tabs.addTab("Assembler", asmCode);
    tabs.setMnemonicAt(1, KeyEvent.VK_2);
    
    commandLn = new JTextPane();
    commandLn.setContentType("text/plain");
    commandLn.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
    commandLnScroll = new JScrollPane(commandLn);
    commandLn.setEditable(false);
    commandLn.setBackground(Color.BLACK);
    commandLn.setForeground(Color.WHITE);
    tabs.addTab("Commandline", commandLnScroll);
    tabs.setMnemonicAt(2, KeyEvent.VK_3);
    
    add(tabs, BorderLayout.CENTER);
    
    JToolBar toolbar = new JToolBar();
    btnOpen = new JButton("Open");
    btnOpen.setMnemonic(KeyEvent.VK_O);
    btnOpen.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        open();
      }
    });
    btnReload = new JButton("Reload");
    btnReload.setMnemonic(KeyEvent.VK_R);
    btnReload.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        loadFiles();
      }
    });
    btnSave = new JButton("Save");
    btnSave.setMnemonic(KeyEvent.VK_S);
    btnSave.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        save();
      }
    });
    btnCompile = new JButton("Compile");
    btnCompile.setMnemonic(KeyEvent.VK_C);
    btnCompile.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        compile(false);
      }
    });
    btnAssemble = new JButton("Assemble");
    btnAssemble.setMnemonic(KeyEvent.VK_A);
    btnAssemble.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        compile(true);
      }
    });
    btnExecute = new JButton("Execute");
    btnExecute.setMnemonic(KeyEvent.VK_X);
    btnExecute.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        runProgram();
      }
    });
    String[] targets = {"LLVM","JVM","x86"};
    cmbTarget = new JComboBox<String>(targets);
    cmbTarget.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        loadFiles();
      }
    });
    toolbar.add(btnOpen);
    toolbar.add(btnReload);
    toolbar.add(btnSave);
    toolbar.add(btnCompile);
    toolbar.add(btnAssemble);
    toolbar.add(btnExecute);
    toolbar.add(cmbTarget);
    add(toolbar, BorderLayout.PAGE_START);
    lblStatus = new JLabel("");
    add(lblStatus, BorderLayout.SOUTH);
    pack();
    
    loadSettings();
    loadFiles();
    
    setTitle("Javalette Development Environment");
    setDefaultCloseOperation(EXIT_ON_CLOSE);
    setSize(800,600);
    setVisible(true);
  }
  
  private void open() {
    JFileChooser chooser = new JFileChooser();
    chooser.setFileFilter(new FileNameExtensionFilter("Javalette source files", "jl"));
    int returnVal = chooser.showOpenDialog(this);
      if(returnVal == JFileChooser.APPROVE_OPTION) {
        fileInUse = new File(chooser.getSelectedFile().getAbsolutePath());
        loadFiles();
      }
  }
  
  private void loadFiles() {
    Scanner sc=null;
    try {
        sc = new Scanner(fileInUse);
        String in = "";
        while(sc.hasNext()) {
          in += sc.nextLine() + "\n";
        }
        sc.close();
        rawCode.setText(in);
        
        String path = fileInUse.getAbsolutePath().substring(0, fileInUse.getAbsolutePath().lastIndexOf('.'));
        if(cmbTarget.getSelectedItem().equals("JVM")) {
          path+=".j";
        } else if(cmbTarget.getSelectedItem().equals("LLVM")) {
          path+=".ll";
        } else if(cmbTarget.getSelectedItem().equals("x86")) {
          path+=".nasm";
        }
        
        asmFileInUse = new File(path);
        
        if(asmFileInUse.exists()) {
          sc = new Scanner(asmFileInUse);
          in = "";
          while(sc.hasNext()) {
            in += sc.nextLine() + "\n";
          }
          asmCode.setText(in);
        } else {
          asmCode.setText("");
          asmFileInUse=null;
        }
        
        String cpath = fileInUse.getAbsolutePath().substring(0, fileInUse.getAbsolutePath().lastIndexOf('.'));
        if(cmbTarget.getSelectedItem().equals("JVM")) {
          cpath+=".class";
        } else if(cmbTarget.getSelectedItem().equals("LLVM")) {
          cpath+=".bc";
        } else if(cmbTarget.getSelectedItem().equals("x86")) {
          cpath+=".out";
        }
        compiledFileInUse = new File(cpath);
      } catch (Exception e) {
        fileInUse=null;
        asmFileInUse=null;
      }
    if(sc!=null)
      sc.close();
    
    String status="";
    if(fileInUse==null||!fileInUse.exists()) {
      fileInUse=null;
       btnSave.setEnabled(false);
       btnCompile.setEnabled(false);
       rawCode.setText("");
       rawCode.setEditable(false);
      asmCode.setText("");
      asmCode.setEditable(false);
      status = "No file open";
    } else {
      status = "File open: "+fileInUse.getAbsolutePath();
      btnSave.setEnabled(true);
       btnCompile.setEnabled(true);
       rawCode.setEditable(true);
       asmCode.setEditable(true);
    }
    
    if(compiledFileInUse!=null&&compiledFileInUse.exists()) {
      btnExecute.setEnabled(true);
    } else {
      btnExecute.setEnabled(false);
    }
    
    if(compiler!=null&&compiler.exists()) {
      status+=" - Compiler in use: " + compiler.getAbsolutePath();
    } else {
      status+=" - No compiler in use.";
    }
    
    lblStatus.setText(status);
    saveSettings();
  }
  
  private void save() {
    FileWriter out;
    String status = "";
    try {
      if(fileInUse!=null&&fileInUse.exists()) {
        out = new FileWriter(fileInUse);
        out.write(rawCode.getText());
        out.close();
        status += fileInUse.getName() + " ";
      }
      
      if(asmFileInUse!=null&&asmFileInUse.exists()) {
        out = new FileWriter(asmFileInUse);
        out.write(asmCode.getText());
        out.close();
        if(!status.equals(""))
          status += "and ";
        status += asmFileInUse.getName() + " ";
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
    if(status.equals(""))
      status = "Nothing ";
    lblStatus.setText("has been saved to disk.");
  }
  
  private void compile(boolean onlyAssemble) {
    save();
    if(compiler==null || !compiler.exists()) {
      compiler=null;
      JOptionPane.showMessageDialog(this, "Compiler not chosen, please locate compiler.");
      JFileChooser chooser = new JFileChooser();
      int returnVal = chooser.showOpenDialog(this);
        if(returnVal == JFileChooser.APPROVE_OPTION) {
          compiler = new File(chooser.getSelectedFile().getAbsolutePath());
          compileFile(onlyAssemble);
        }
    } else {
      compileFile(onlyAssemble);
    }
    loadFiles();
  }
  
  private void compileFile(boolean onlyAssemble) {
    Scanner scerr = null;
    Scanner scout = null;
      try 
      { 
        String commandlnText = "==Start compiling==\n";
        String flags = "";
        if(onlyAssemble) {
          flags += " -a ";
        }
        if(cmbTarget.getSelectedItem().equals("JVM")) {
          flags += " -t jvm ";
        } else if (cmbTarget.getSelectedItem().equals("LLVM")) {
          flags += " -t llvm ";
        } else if (cmbTarget.getSelectedItem().equals("x86")) {
          flags += " -t x86 ";
        }
        Path inputPath = Paths.get(fileInUse.getAbsolutePath());
        String fileName = Paths.get(compiler.getParent()).relativize(inputPath).toString().replace('\\', '/');
        
        Process p;
        if(System.getProperty("os.name").startsWith("Windows")) {
          p = Runtime.getRuntime().exec(new String[] {"cmd", "/c", "cd "+compiler.getParent()+"; ./jlc"+flags+fileName}); 
        } else {
          p = Runtime.getRuntime().exec(new String[] {"/bin/sh", "-c", "cd "+compiler.getParent()+"; ./jlc"+flags+fileName}); 
        }
        p.waitFor();
        
        commandlnText+="\n==stderr==\n";
        scerr = new Scanner(p.getErrorStream()); 
        while(scerr.hasNext()) 
        {
          commandlnText+=scerr.nextLine()+"\n";
        }
        commandlnText+="\n==stdout==\n";
        scout = new Scanner(p.getInputStream()); 
        while(scout.hasNext()) 
        { 
          commandlnText+=scout.nextLine()+"\n";
        } 
        commandlnText+="\n==Compilation done!==\n\n------------------------------------------------------------------------------\n\n";
        printToTerminal(commandlnText);
        saveSettings();
        if(cmbTarget.getSelectedItem().equals("JVM")) {
          compiledFileInUse = new File(fileInUse.getPath().substring(0, fileInUse.getPath().lastIndexOf('.')) + ".class");
        } else if (cmbTarget.getSelectedItem().equals("LLVM")) {
          compiledFileInUse = new File(fileInUse.getParent()+"/a.out.bc");
          System.out.println(compiledFileInUse.getPath());
        } else if (cmbTarget.getSelectedItem().equals("x86")) {
          compiledFileInUse = new File(fileInUse.getParent()+"/a.out");
        }
        
      } 
      catch(Exception e) {
        e.printStackTrace();
        System.out.println("Could not use compiler at: "+compiler.getAbsolutePath());
      }
      if(scerr!=null)
        scerr.close();
      if(scout!=null)
        scout.close();
  }
  
  private void runProgram() {
    Scanner sc = null;
    try {
      String separator = ":";
      if(System.getProperty("os.name").startsWith("Windows")) {
          separator=";";
      }
      String className = compiledFileInUse.getName().substring(0, compiledFileInUse.getName().lastIndexOf('.'));
      String commandlnText = "==Start run of "+className+" using " + cmbTarget.getSelectedItem() + "==\n";
      Path filePath = Paths.get(fileInUse.getParent());
      
      Process p = null;
      if(cmbTarget.getSelectedItem().equals("JVM")) { //JVM
        Path currentPath = Paths.get(new java.io.File("").getAbsolutePath());
        Path libPath = Paths.get(compiler.getParent()+"/lib");
        String cp = "."+separator+currentPath.relativize(libPath).toString().replace('\\', '/')
                     +separator + currentPath.relativize(filePath).toString().replace('\\', '/');
        if(System.getProperty("os.name").startsWith("Windows")) {
          p = Runtime.getRuntime().exec(new String[] {"cmd /c", "java", "-cp", cp, className});
        } else {
          p = Runtime.getRuntime().exec(new String[] {"java", "-cp", cp, className});
        }
      } else if(cmbTarget.getSelectedItem().equals("LLVM")) { //LLVM
        if(System.getProperty("os.name").startsWith("Windows")) {
          p = Runtime.getRuntime().exec(new String[] {"cmd /c", "lli", filePath.toString() + "/" + className + ".bc"});
        } else {
          p = Runtime.getRuntime().exec(new String[] {"/bin/sh", "-c", "cd "+filePath.toString()+"; lli a.out.bc"});
          System.out.println("cd "+filePath.toString()+"; ./a.out");
        }
      } else if(cmbTarget.getSelectedItem().equals("x86")) { //x86
        JOptionPane.showMessageDialog(this, "x86 not supported by compiler yet");
        return;
      }
      p.waitFor();
      sc = new Scanner(p.getInputStream()); 
      while(sc.hasNext()) { 
        commandlnText+=sc.nextLine()+"\n";
      } 
      commandlnText+="==Program finished running==\n\n------------------------------------------------------------------------------\n\n";
      printToTerminal(commandlnText);
    } catch (Exception e) {
      e.printStackTrace();
    } 
    if(sc!=null)
      sc.close();
  }
  
  private void printToTerminal(String text) {
    tabs.setSelectedIndex(2);
    commandlnLog+=text;
    commandLn.setText(commandlnLog);
      JScrollBar vertical = commandLnScroll.getVerticalScrollBar();
      vertical.setValue(vertical.getMaximum());
  }
  
  private void saveSettings() {
    FileWriter out;
    try {
      out = new FileWriter("settings.ini");
      if(fileInUse!=null&&fileInUse.exists())
        out.write(fileInUse.getAbsolutePath()+"\n");
      else
        out.write("null\n");
      if(compiler!=null&&compiler.exists())
        out.write(compiler.getAbsolutePath()+"\n");
      else
        out.write("null\n");
      out.flush();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
  
  private void loadSettings() {
    Scanner sc = null;
    try {
      sc = new Scanner(new File("settings.ini"));
      fileInUse = new File(sc.nextLine());
      compiler = new File(sc.nextLine());
    } catch (Exception e) {
      System.out.println("Could not read settingsfile. Skipping.");
      fileInUse=null;
      compiler=null;
    } 
    if(sc!=null)
      sc.close();
  }
  
  public static void main(String[] args) {
    new JLDE();
  }
}
