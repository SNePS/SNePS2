package edu.buffalo.sneps;
import java.util.HashMap;

/**
 * Represents a substitution (a set of pairs, each of which is a variable and a term). 
 * Used by the {@link edu.buffalo.sneps.JavaSnepsAPI#askwh(String) askwh} and {@link edu.buffalo.sneps.JavaSnepsAPI#askwhnot(String) askwhnot} methods to represent substitutions. A HashSet of these are returned by these methods as there can be multiple substitutions that satisfy a particular query.
 **/
public class Substitution extends HashMap<String,String>{

    // Change to a HashMap
	
    /**
     * Creates an instance of the substitution with the specified capacity (number of substitution pairs).
     * @param capacity The capacity for this substitution
     **/
    public Substitution (int capacity) {
	super(capacity);
    }


    /**
     * Adds a substitution pair to this substitution of the specified variable and value.
     * @param var The variable identifier.
     * @param val The value that unifies with that variable
     **/
    public void addSubstitutionPair(String var, String val) {
	put(var,val);
    }

    /** Returns the value that unifies with the specified variable in this substitution.
     * @param var The variable identifier to retrieve the value from.
     * @return The value that unifies with <code>var</code>, or the empty string is no such variable is found in this set.
     **/
    public String getValFromVar (String var) {
		
	return get(var);
    }

}
