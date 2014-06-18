import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Scanner;
import java.util.TreeSet;

/**
 * Uniform sampling of a large network.
 * The network takes the form of an
 * edge list. This was implemented to
 * allow generating hive plots (otherwise
 * the whole network was too big). 
 * 
 * @author Vincent Labatut
 */
public class ApplySampling
{
	/////////////////////////////////////////////////////////////////
	// MAIN				/////////////////////////////////////////////
	/////////////////////////////////////////////////////////////////
	public static void main(String[] args) throws FileNotFoundException
	{	// set file names
		//String path = "~/eclipse/workspaces/Networks/Orleans/data/";
		String path = "/home/vlabatut/eclipse/workspaces/Networks/Orleans/data/";
		String networkFile = "links-anon.txt";
		String sampleFile = "sample.txt";
		String nodeSeparator = " ";
		
		// read sample
		String sFile = path + sampleFile;
		TreeSet<Long> sample = loadSample(sFile);
		
		// open input file
		String inFile = path + networkFile;
		Scanner scanner = openInputFile(inFile);
		
		// open output file
		String outFile = path + networkFile + ".sampled";
		PrintWriter printWriter = openOutputFile(outFile);

		// date formatting stuff
		DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
		
		// process file
		System.out.println("Extracting sample from network");
		int count = 0;
		long min = Long.MAX_VALUE;
		while(scanner.hasNextLine())
		{	if(count%1000000==0)
			{	Calendar cal = Calendar.getInstance();
				String date = dateFormat.format(cal.getTime());
				System.out.println(".. "+date+": "+count+" links processed");
			}
			String line = scanner.nextLine();
			String[] str = line.split(nodeSeparator);
			long from = Long.parseLong(str[0]);
			long to = Long.parseLong(str[1]);
			if(sample.contains(from)  && sample.contains(to))
				printWriter.println(line);
			if(from<min)
				min = from;
			if(to<min)
				min = to;
			count++;
		}
		
		// close streams
		scanner.close();
		printWriter.close();
		System.out.println("Smaller index found: "+min);
	}

	/**
	 * Reads the file containing the numbers of
	 * the nodes to be sampled.
	 * 
	 * @param file
	 * 		Sample file.
	 * @return
	 * 		List containing the sampled nodes.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while accessing the file.
	 */
	private static TreeSet<Long> loadSample(String file) throws FileNotFoundException
	{	System.out.println("Reading sample");
		Scanner scanner = openInputFile(file);
		TreeSet<Long> result = new TreeSet<Long>();
		while(scanner.hasNextLong())
		{	long value = scanner.nextLong();
			result.add(value);
		}
		scanner.close();
		System.out.println("..Number of values read: "+result.size());
		return result;
	}
	
	/**
	 * Open a file in read mode.
	 * 
	 * @param file
	 * 		File to be opened.
	 * @return
	 * 		Scanner object pointing at the begining of the file.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while accessing the file.
	 */
	private static Scanner openInputFile(String file) throws FileNotFoundException
	{	FileInputStream fileIn = new FileInputStream(file);
		BufferedInputStream buffIn = new BufferedInputStream(fileIn);
		InputStreamReader reader = new InputStreamReader(buffIn);
		Scanner result = new Scanner(reader);
		return result;
	}
	
	/**
	 * Open a file in write mode.
	 * 
	 * @param file
	 * 		File to be opened.
	 * @return
	 * 		PrintWriter object pointing on an empty file.
	 * 
	 * @throws FileNotFoundException
	 * 		Problem while accessing the file.
	 */
	private static PrintWriter openOutputFile(String file) throws FileNotFoundException
	{	FileOutputStream fileOut = new FileOutputStream(file);
		OutputStreamWriter writer = new OutputStreamWriter(fileOut);
		PrintWriter result = new PrintWriter(writer);
		return result;
	}
}
