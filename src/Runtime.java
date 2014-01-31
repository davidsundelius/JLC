import java.util.Scanner;

class Runtime {
	private static Scanner sc = new Scanner(System.in);

	public static void printInt(int i) {
		printString(String.valueOf(i));
	}
	public static void printDouble(double d) {
		printString(String.valueOf(d));
	}
	public static void printString(String s) {
		System.out.println(s);
	}

	public static int readInt() {
		return sc.nextInt();
	}

	public static double readDouble() {
		return sc.nextDouble();
	}
}
