package pins.common.logger;

/**
 * Implemented by objects that a log should be produced for.
 */
public interface Loggable {

	/**
	 * Produces a log of this object.
	 * 
	 * @param pfx String to be printed in front of the log.
	 */
	public void log(String pfx);

}
