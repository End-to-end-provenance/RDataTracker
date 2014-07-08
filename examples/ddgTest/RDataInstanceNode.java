package laser.ddg.r;

import laser.ddg.AbstractDataInstanceNode;

/**
 * A data node representing data produced by an R script.
 * 
 * @author Barbara Lerner
 * @version Jul 8, 2013
 *
 */
public class RDataInstanceNode extends AbstractDataInstanceNode{
	// The node type. 
	private String type;
	
	/**
	 * Creates a new data node
	 * @param type the node type -- one of "Data", "File" or "URL" 
	 * @param name the data name
	 * @param value the data value
	 * @param time the time that the data was created or the timestamp associated
	 * 		with a file used as input
	 * @param location the original location for a file node, null if not a file node
	 */
	public RDataInstanceNode(String type, String name, String value, String time, String location){		
		super(value, name, time, location);
		this.type = type;
	}

	@Override
	public String getType() {
		return type;
	}
}
