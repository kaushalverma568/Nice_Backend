/**
 *
 */
package com.nice.repository;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Repository;

import com.nice.config.UserAwareUserDetails;
import com.nice.constant.UserType;
import com.nice.model.Ticket;
import com.nice.model.UserLogin;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Aug 6, 2020
 */
@Repository(value = "ticketCustomRepository")
public class TicketCustomRepositoryImpl implements TicketCustomRepository {

	@PersistenceContext
	private EntityManager entityManager;

	@SuppressWarnings("unchecked")
	@Override
	public List<Ticket> getTicketListBasedOnParams(Long entityId, String userType, final String name, final Integer startIndex, final Integer pageSize) {

		/**
		 * for delivery boy,customer and vendor we will give his/her tickets only
		 */
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (UserType.CUSTOMER.name().equals(userLogin.getEntityType()) || UserType.VENDOR.name().equals(userLogin.getEntityType())
				|| UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType())) {
			userType = userLogin.getEntityType();
			entityId = userLogin.getEntityId();
		}

		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder(
				"select t.* as tic from ticket t left join customer ct on t.entity_id=ct.id and t.user_type = 'CUSTOMER' left join delivery_boy db on t.entity_id=db.id and t.user_type = 'DELIVERY_BOY' left join vendor vd on t.entity_id=vd.id and t.user_type = 'VENDOR' where 1=1 ");

		addConditions(entityId, userType, name, sqlQuery, paramMap);

		if (startIndex != null && pageSize != null) {
			sqlQuery.append(" offset :startIndex  limit :pageSize ");
			paramMap.put("startIndex", startIndex);
			paramMap.put("pageSize", pageSize);

		}

		Query q = entityManager.createNativeQuery(sqlQuery.toString(), "TicketMapping");
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return q.getResultList();
	}

	@Override
	public Long getTicketCountBasedOnParams(Long entityId, String userType, final String name) {
		/**
		 * for delivery boy,customer and vendor we will give his/her tickets count only
		 */
		UserLogin userLogin = ((UserAwareUserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal()).getUser();
		if (UserType.CUSTOMER.name().equals(userLogin.getEntityType()) || UserType.VENDOR.name().equals(userLogin.getEntityType())
				|| UserType.DELIVERY_BOY.name().equals(userLogin.getEntityType())) {
			userType = userLogin.getEntityType();
			entityId = userLogin.getEntityId();
		}

		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder(
				"select count(t.*)as count1  from ticket t left join customer ct on t.entity_id=ct.id and t.user_type = 'CUSTOMER' left join delivery_boy db on t.entity_id=db.id and t.user_type = 'DELIVERY_BOY' left join vendor vd on t.entity_id=vd.id and t.user_type = 'VENDOR' where 1=1 ");
		addConditions(entityId, userType, name, sqlQuery, paramMap);

		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return ((BigInteger) q.getSingleResult()).longValue();

	}

	/**
	 * @param  ticketParamRequestDTO
	 * @param  sqlQuery
	 * @param  paramMap
	 * @return
	 */
	private StringBuilder addConditions(final Long entityId, final String userType, final String name, final StringBuilder sqlQuery,
			final Map<String, Object> paramMap) {
		if (entityId != null) {
			sqlQuery.append(" and t.entity_id = :entityId ");
			paramMap.put("entityId", entityId);
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(userType)) {
			sqlQuery.append(" and t.user_type = :userType ");
			paramMap.put("userType", userType);
		}
		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(name)) {
			sqlQuery.append(
					" and(lower(CONCAT (ct.first_name,' ',ct.last_name)) like :name  or lower(CONCAT (db.first_name,' ',db.last_name)) like :name or lower(CONCAT (vd.first_name,' ',vd.last_name)) like :name )");
			paramMap.put("name", "%" + name.toLowerCase() + "%");
		}
		return sqlQuery;
	}
}
