package edu.buffalo.sneps;


import java.io.IOException;
import java.io.File;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileReader;
import java.io.InputStream;


import com.franz.jlinker.JavaLinkDist;
import com.franz.jlinker.LispCall;
import com.franz.jlinker.TranStruct;

import edu.buffalo.sneps.Substitution;

import java.util.HashSet;


// StreamGobbler class written by Michael C. Daconta, JavaWorld.com, 12/29/00
class StreamGobbler extends Thread
{
    InputStream is;
    String type;
    
    StreamGobbler(InputStream is, String type)
    {
        this.is = is;
        this.type = type;
    }
    
    public void run()
    {
        try
        {
            InputStreamReader isr = new InputStreamReader(is);
            BufferedReader br = new BufferedReader(isr);
            String line=null;
            while ( (line = br.readLine()) != null) {
                
		// Uncomment the following to print process information 
		//System.out.println(type + ">" + line);
	    }
	} catch (IOException ioe)
	    {
                ioe.printStackTrace();  
	    }
    }
}

/**
 * Class of connections between Java and the SNePS
 * knowledge representation and reasoning system. Methods defined
 * invoke their respective SNePS functions using Allegro Common Lisp's (ACL) 
 * Jlinker interface
 *<p>
 * Note: ACL or an instance of the SNePS executable is required to use this 
 * API.  
 * @see <a href="http://www.franz.com/support/documentation/8.0/doc/jlinker.htm">jLinker - A Dynamic Link between Lisp and Java</a>
 * @author Michael Kandefer
 **/
public class JavaSnepsAPI
{
   
    /**
     * The connection between Java and Lisp (SNePS)
     **/
    private JavaLispConnection jlc;
  

    /**
     * Creates an instance of the JavaSnepsAPI using the specified port. It is 
     * presumed that the user will manually start up the SNePS system and 
     * invoke the SNePS connection function as follows: 
     * <p>
     * (snepslog:init-java-sneps-connection
     * <code>interface_port</code> <code>java-classpath</code>) <p>The
     * java-classpath is the classpath needed to compile and run this
     * class. 
     * @param interface_port The port to use for this connection
     **/
    public JavaSnepsAPI (int interface_port) {
	jlc = new JavaLispConnection(interface_port);
	(new Thread(jlc)).start();
	while(!isConnected()){
	    try {
		Thread.sleep(1000);
		 
	    } catch (InterruptedException e) {
		e.printStackTrace();
	    } // end of try-catch
	    
	}
    }

	
    /**
     * Creates an instance of the JavaSnepsAPI using the specified port and config_file. Starts SNePS using information specified in the config_file.
     * <p>
     * @param config_file Location of the JavaSNePS config file (java_sneps_config.config). Typically this file is the JavaSneps subdirectory in the SNePS home directory. Ask the administrator who installed SNePS for its location.
     * @param interface_port The port to use for this connection
     **/
    public JavaSnepsAPI(String config_file, int interface_port)
    {
	String sneps_config = "";
	String sneps_command = "";
	try {
	    BufferedReader in = 
		new BufferedReader(new FileReader(config_file));
	    String str;
	    
	    while ((str = (in.readLine())) != null) {
		str = str.trim();
		if(!(str.startsWith(";;;"))){
		    String [] var_val = str.split("=");
		    if(var_val[0].equals("SNEPS_CONFIG_FILE")){
			sneps_config = var_val[1]; 
		    }
		    else if(var_val[0].equals("SNEPS_COMMAND")){
			sneps_command = var_val[1];
		    }
		}
	    }
	    in.close();
	} 
	catch (IOException e) {
	    e.printStackTrace();
	}

	//com.franz.jlinker.JavaLinkCommon.sdebug = true;
	jlc = new JavaLispConnection(interface_port);
	(new Thread(jlc)).start();

	try{

	    Runtime rt = Runtime.getRuntime();
	    Process proc = rt.exec(sneps_command + " -- -config-file " + 
				   sneps_config +
				   " -api-port " + interface_port +
				   " -api-classpath " + 
				   System.getProperty("java.class.path"));
	    // any error message?
	    StreamGobbler errorGobbler = new 
		StreamGobbler(proc.getErrorStream(), "ERROR");            
	    
	    // any output?
	    StreamGobbler outputGobbler = new 
		StreamGobbler(proc.getInputStream(), "OUTPUT");
	    
	    // kick them off
	    errorGobbler.start();
	    outputGobbler.start();
	}
	catch (IOException e) {
	    e.printStackTrace();
	}
	while(!jlc.isConnected()){
	    try {
		Thread.sleep(1000);
		 
	    } catch (InterruptedException e) {
		e.printStackTrace();
	    } // end of try-catch
	    
	}    
	//loadNetworkTerrainData(DEFAULT_TERRAIN);
    }
    
    
    /**
     * Returns true if the connection between Java and Sneps is still functioning.
     @return <code>true</code> if Java and SNePS are still connected, <code>false</code> otherwise.
    **/
    public boolean isConnected()
    {
	return jlc.isConnected();
    }
    
    
    /** Given a proper command string, invokes the SNePSLOG tell method on that command. 
     * <p> Note: Unlike the SNePS version of tell, this tell returns nothing. 
     * @param command A SNePSLOG parsable string to invoke in SNePS. 
     * @see "SNePS 2.7.0 Manual - snepslog:tell"
     **/
    public void tell (String command)
    {
	LispCall snepslog_tell = new LispCall("snepslog:tell");
	snepslog_tell.addArg(command);
	snepslog_tell.setStyle(LispCall.STYLE_IGNORE);
	try {
	    snepslog_tell.call();
	}
	catch (JavaLinkDist.JLinkerException ex) {
	    ex.printStackTrace();
	} // end of catch
	snepslog_tell.close();
	snepslog_tell.reset();
    }
    
    
     
    /** Given a valid ask input, invokes the SNePSLOG ask method on that input. Returns a HashSet<String> of strings representing the node results.
     * @param command Valid ask input with which to query SNePS.
     * @return A {@link java.util.HashSet} of String objects  representing the query results 
     * @see "The SNePS 2.7.0 Manual - snepslog:ask"
     **/
    public HashSet<String> ask (String command)
    {
	HashSet<String> result = null;
	TranStruct ts = null;
	LispCall snepslog_ask = new LispCall("snepslog:java-sneps-ask");
	snepslog_ask.addArg(command);
	snepslog_ask.setStyle(LispCall.STYLE_COPY);
	try {
	    snepslog_ask.call();
	    result = (HashSet<String>)snepslog_ask.objectValue();
	}
	catch (JavaLinkDist.JLinkerException ex) {
	    ex.printStackTrace();
	} // end of catch
	snepslog_ask.close();
	snepslog_ask.reset();
	
	return result;
    }

    /** Given a valid ask input, invokes the SNePSLOG askifnot method on that input. Returns a HashSet<String> of strings representing the node results.
     * @param command Valid ask input with which to query SNePS. 
     * @return A {@link java.util.HashSet} of String objects representing query results 
     * @see "The SNePS 2.7.0 Manual - snepslog:askifnot"
     **/
    public HashSet<String> askifnot (String command)
    {
	HashSet<String> result = null;
	TranStruct ts = null;
	LispCall snepslog_ask = new LispCall("snepslog:java-sneps-askifnot");
	snepslog_ask.addArg(command);
	snepslog_ask.setStyle(LispCall.STYLE_COPY);
	try {
	    snepslog_ask.call();
	    result = (HashSet<String>)snepslog_ask.objectValue();
	}
	catch (JavaLinkDist.JLinkerException ex) {
	    ex.printStackTrace();
	} // end of catch
	snepslog_ask.close();
	snepslog_ask.reset();
	
	return result;
    }
    
    /** Given a valid ask input, invokes the SNePSLOG askwh method on that input. Returns a HashSet of the substitution objects representing the variable substitutions needed to answer the query.
     * @param command Valid ask input with which to query SNePS. 
     * @return {@link java.util.HashSet} of {@link edu.buffalo.sneps.Substitution}
     * @see edu.buffalo.sneps.Substitution#getValFromVar(String) "The get value from variable method"
     * @see "The SNePS 2.7.0 Manual - snepslog:askwh"
     **/
    public HashSet<Substitution> askwh (String command)
    {
	//System.out.println("Asking the system (askwh): "+ command);
	HashSet<Substitution> result = null;
	LispCall snepslog_ask = new LispCall("snepslog:java-sneps-askwh");
	snepslog_ask.addArg(command);
	snepslog_ask.setStyle(LispCall.STYLE_COPY);
	try {
	    snepslog_ask.call();
	    result = (HashSet<Substitution>)snepslog_ask.objectValue();
	}
	catch (JavaLinkDist.JLinkerException ex) {
	    ex.printStackTrace();
	} // end of catch

	snepslog_ask.close();
	snepslog_ask.reset();
	return result;
    } 
    
    /** Given a valid ask input, invokes the SNePSLOG askwhnot method on that input. Returns a HashSet<Substitution> of substitution objects representing the variable substitutions needed to answer the query.
     * @param command Valid ask input with which to query SNePS. 
     * @return  {@link java.util.HashSet} of {@link edu.buffalo.sneps.Substitution}
     * @see edu.buffalo.sneps.Substitution#getValFromVar(String) "The get value from variable method"
     * @see "The SNePS 2.7.0 Manual - snepslog:askwhnot"
     **/
    public HashSet<Substitution> askwhnot (String command)
    {
	//System.out.println("Asking the system (askwh): "+ command);
	HashSet<Substitution> result = null;
	LispCall snepslog_ask = new LispCall("snepslog:java-sneps-askwhnot");
	snepslog_ask.addArg(command);
	snepslog_ask.setStyle(LispCall.STYLE_COPY);
	try {
	    snepslog_ask.call();
	    result = (HashSet<Substitution>)snepslog_ask.objectValue();
	}
	catch (JavaLinkDist.JLinkerException ex) {
	    ex.printStackTrace();
	} // end of catch

	snepslog_ask.close();
	snepslog_ask.reset();
	return result;
    }
    
    
    /**
     * Terminates the Java-Sneps connection. 
     **/
    public void endLispConnection ()
    {
	LispCall jlinker_end = new LispCall("snepslog:jlinker-end");
	jlinker_end.setStyle(LispCall.STYLE_ONEWAY);
	try {
	    jlinker_end.call();
	}
	catch (JavaLinkDist.JLinkerException ex) {
	    ex.printStackTrace();
	} // end of catch
	jlinker_end.close();
	jlinker_end.reset();
    }
	
    //public static void main(String [] args){
    //	System.out.println("Classpath is: " + System.getProperty("java.class.path"));
    //}

    /**
     * Represents the connection between Java and Lisp. Used primarily to start a listening thread that waits for the SNePS application to connect.
     **/	
    private class JavaLispConnection implements Runnable
    {
	/**
	 * Specifies if the connection has been established.
	 **/
	private boolean connected = false;
	
	/**
	 * The port to listen on.
	 **/
	private int jlinker_port = -1;
	
	/** 
	 * Create an instance of the Java Lisp connection on the specified port.
	 *@param p The port to listen on.
	 **/ 
	public JavaLispConnection(int p){
	    jlinker_port = p;
	}
	
	/**
	 * Returns the status of the connection.
	 * @return <code>true</code> if connected, <code>false</code> otherwise
	 **/
	public boolean isConnected() {
	    return connected;
	}
	
	/**
	 * The <code>run</cpde> method must be specified when implementing Runnable. This particular <code>run</code> method starts the jlinker listener, which waits for a Lisp process to connect to it. 
	 * @see  <a href="http://www.franz.com/support/documentation/8.0/doc/jlinker.htm#dl-java-ref-2">jLinker - Dynamic Linkage Java Reference</a>
	 **/
	public void run() {
            System.out.println("Waiting for lisp connection... ");
	    //com.franz.jlinker.LispConnector lc = 
	    //	new com.franz.jlinker.LispConnector();
	    //lc.lispAdvertises = false;
	    //lc.advertInFile = false;
	    //lc.javaPort = jlinker_port;
	    //lc.javaTimeout = -1;
	    //lc.go();
	    com.franz.jlinker.JavaLinkDist.advertise(jlinker_port, -1); 
	    System.out.println("Connection received!");
            connected = true;
	}
    }
}
