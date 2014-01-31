import java.awt.BorderLayout;
import java.awt.Font;

import javax.swing.JEditorPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import jsyntaxpane.DefaultSyntaxKit;



public class JLEditor extends JPanel {
	public static enum CodeMode {
		JAVALETTE, JASM, LLVM, NASM;
	}
	
	private static final long serialVersionUID = 2L;
	private JEditorPane editor;
	private JScrollPane scroller;
	
	public JLEditor() {
		super();
		setup();
	}
	
	private void setup() {
		setLayout(new BorderLayout());
		DefaultSyntaxKit.initKit();
		
		editor = new JEditorPane();
		scroller = new JScrollPane(editor);
		add(scroller, BorderLayout.CENTER);
		editor.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));
		editor.setContentType("text/java");
		setCodeMode(CodeMode.JAVALETTE);
	}
	
	public void setText(String text) {
		editor.setText(text);
	}
	
	public String getText() {
		return editor.getText();
	}
	
	public void setEditable(boolean b) {
		return;
	}
	
	public void setCodeMode(CodeMode cm) {
		switch (cm) {
			case JAVALETTE:
				break;
			default:
				break;
		}
	}
}