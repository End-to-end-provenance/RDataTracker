package rDataTracker;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.net.ServerSocket;
import java.net.Socket;

public class WriteUtility {

	public static void main(String[] args) {
		// if (args.length == 0) {
		// return;
		// }
		//
		// String outfileName = args[0];

		int portNumber = 20140;
		ServerSocket serverSocket = null;

		try {

			// System.out.println("Creating socket");
			serverSocket = new ServerSocket(portNumber);

			while (true) {
				// System.out.println("Waiting for client");
				Socket clientSocket = serverSocket.accept();

				// System.out.println("Client connected");
				new WriteThread(clientSocket).start();
			}
		} catch (IOException e) {
			System.out.println(e);
		} finally {
			if (serverSocket != null) {
				try {
					serverSocket.close();
				} catch (Exception e) {
					System.out.println(e);
				}
			}
		}

	}

	public void writeToFile(final String outFileName, final String data) {
		new Thread() {
			public void run() {
				//Thread.yield();  // Didn't help
				PrintWriter outFile = null;
				try {
					System.out.println("Writing to file " + outFileName);
					outFile = new PrintWriter(new File(outFileName));
					outFile.println(data);
				} catch (IOException e) {
					System.out.println("File writing failed: " + e);
				} finally {
					// try {
					if (outFile != null) {
						outFile.close();
					}
					// } catch (IOException e) {
					// System.out.println("File writing failed: " + e);
					// }
				}
			}
		}.start();

	}

	public void writeToFile(final String outFileName, final String[] data) {
		new Thread() {
			public void run() {
				Thread.yield();
				System.out.println("Java writing " + outFileName);
				PrintWriter outFile = null;
				try {
					System.out.println("Writing to file " + outFileName);
					outFile = new PrintWriter(new File(outFileName));
					for (int i = 0; i < data.length; i++) {
						outFile.println(data[i]);
					}
				} catch (IOException e) {
					System.out.println("File writing failed: " + e);
				} finally {
					// try {
					if (outFile != null) {
						outFile.close();
					}
					// } catch (IOException e) {
					// System.out.println("File writing failed: " + e);
					// }
				}
				System.out.println("Java done writing " + outFileName);
			}
		}.start();

	}

}

class WriteThread extends Thread {
	private Socket clientSocket;

	public WriteThread(Socket clientSocket) {
		this.clientSocket = clientSocket;
	}

	public void run() {
		Writer outFile = null;
		try {
			PrintWriter out = new PrintWriter(clientSocket.getOutputStream(),
					true);
			BufferedReader in = new BufferedReader(new InputStreamReader(
					clientSocket.getInputStream()));

			String inputLine, outputLine;

			String outFileName = in.readLine();
			System.out.println("Writing to file " + outFileName);
			System.out
					.println("** Warning: file writing code is commented out.");

			String dataSizeString = in.readLine();
			try {
				int dataSize = Integer.parseInt(dataSizeString);
				System.out.println("Java received data size: " + dataSize);
				BufferedWriter bufferedOutFile = new BufferedWriter(
						new FileWriter(outFileName));
				outFile = bufferedOutFile;
				char[] buffer = new char[dataSize];
				int charsRead = in.read(buffer, 0, dataSize);

				while (charsRead >= 0) {
					// System.out.println("Java writing " + charsRead +
					// " bytes");
					// bufferedOutFile.write(buffer, 0, charsRead);
					charsRead = in.read(buffer, 0, dataSize);
				}
			} catch (NumberFormatException e) {
				System.out.println("Java did not receive data size: "
						+ dataSizeString);
				PrintWriter printOutFile = new PrintWriter(
						new File(outFileName));
				outFile = printOutFile;
				System.out.println("Java writing line-by-line");

				// System.out.println("Received from client");
				// printOutFile.println(dataSizeString);
				while ((inputLine = in.readLine()) != null) {
					// printOutFile.println(inputLine);
				}
			}
			System.out.println("Java done writing " + outFileName);
		} catch (IOException e) {
			System.out.println("Closing connection to client: " + e);
		} finally {
			try {
				clientSocket.close();
				if (outFile != null) {
					outFile.close();
				}
			} catch (IOException e) {
				System.out.println("Closing connection to client: " + e);
			}
		}
	}

}
