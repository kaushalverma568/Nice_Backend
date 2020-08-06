/**
 *
 */
package com.nice.repository;

import java.util.List;

import com.nice.model.Ticket;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 6, 2020
 */
public interface TicketCustomRepository {

	/**
	 *
	 * @param  entityId
	 * @param  userType
	 * @param  name
	 * @return
	 */
	Long getTicketCountBasedOnParams(Long entityId, String userType, String name);

	/**
	 *
	 * @param  entityId
	 * @param  userType
	 * @param  name
	 * @param  startIndex
	 * @param  pageSize
	 * @return
	 */
	List<Ticket> getTicketListBasedOnParams(Long entityId, String userType, String name, Integer startIndex, Integer pageSize);

}
