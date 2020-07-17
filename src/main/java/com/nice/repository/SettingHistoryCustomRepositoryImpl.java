/**
 *
 */
package com.nice.repository;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.stereotype.Repository;

import com.nice.model.SettingHistory;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Repository(value = "settingHistoryCustomRepository")
public class SettingHistoryCustomRepositoryImpl implements SettingHistoryCustomRepository {

     private static final String CREATEAT = "createdAt";
	
	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<SettingHistory> getListBasedOnParams(final Integer startIndex, final Integer pageSize, 
			 String fieldName, Date fromDate, Date toDate) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		CriteriaQuery<SettingHistory> criteriaQuery = criteriaBuilder.createQuery(SettingHistory.class);
		Root<SettingHistory> settingHistory = criteriaQuery.from(SettingHistory.class);
		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();	
		if (fieldName != null) {
			predicates.add(criteriaBuilder.equal(settingHistory.get("fieldName"),fieldName));
		}
		if (fromDate != null) {
			predicates.add(criteriaBuilder.greaterThanOrEqualTo(settingHistory.get(CREATEAT).as(java.sql.Date.class),fromDate));
		}
		if (toDate != null) {
			predicates.add(criteriaBuilder.lessThanOrEqualTo(settingHistory.get(CREATEAT).as(java.sql.Date.class),toDate));
		}

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(settingHistory).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		criteriaQuery.orderBy(criteriaBuilder.desc(settingHistory.get("id")));

		TypedQuery<SettingHistory> query = entityManager.createQuery(criteriaQuery);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}

		return query.getResultList();
	}

	@Override
	public Long getCountBasedOnParams(String fieldName, Date fromDate, Date toDate) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
		Root<SettingHistory> settingHistory = criteriaQuery.from(SettingHistory.class);
		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();	
		if (fieldName != null) {
			predicates.add(criteriaBuilder.equal(settingHistory.get("fieldName"),fieldName));
		}
		if (fromDate != null) {
			predicates.add(criteriaBuilder.greaterThanOrEqualTo(settingHistory.get(CREATEAT).as(java.sql.Date.class),fromDate));
		}
		if (toDate != null) {
			predicates.add(criteriaBuilder.lessThanOrEqualTo(settingHistory.get(CREATEAT).as(java.sql.Date.class),toDate));
		}
		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(criteriaBuilder.count(settingHistory)).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));

		TypedQuery<Long> query = entityManager.createQuery(criteriaQuery);
		return query.getSingleResult();
	}
	
	



}
