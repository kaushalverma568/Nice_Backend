/**
 *
 */
package com.nice.repository;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import javax.persistence.EntityGraph;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.stereotype.Repository;

import com.nice.model.CashCollection;
import com.nice.model.DeliveryBoy;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Repository(value = "cashCollectionCustomRepository")
public class CashCollectionCustomRepositoryImpl implements CashCollectionCustomRepository {


	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<CashCollection> getListBasedOnParams(final Integer startIndex, final Integer pageSize, 
			Long deliveryBoyId, Date createdAt) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		CriteriaQuery<CashCollection> criteriaQuery = criteriaBuilder.createQuery(CashCollection.class);
		Root<CashCollection> cashCollection = criteriaQuery.from(CashCollection.class);
		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();	
		if (deliveryBoyId!= null) {
			Join<CashCollection, DeliveryBoy> deliveryBoy = cashCollection.join("deliveryBoy", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(deliveryBoy.get("id"), deliveryBoyId));
		}
		if (createdAt != null) {
			predicates.add(criteriaBuilder.equal(cashCollection.get("createdAt").as(java.sql.Date.class),createdAt));
		}

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(cashCollection).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		criteriaQuery.orderBy(criteriaBuilder.desc(cashCollection.get("id")));
		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields
		 * which we want to retrieve and lets us choose the graph type at runtime.
		 */
		EntityGraph<CashCollection> fetchGraph = entityManager.createEntityGraph(CashCollection.class);
		fetchGraph.addSubgraph("deliveryBoy");
		TypedQuery<CashCollection> query = entityManager.createQuery(criteriaQuery).setHint("javax.persistence.loadgraph", fetchGraph);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}

		return query.getResultList();
	}

	@Override
	public Long getCountBasedOnParams( Long deliveryBoyId, Date createdAt) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
		Root<CashCollection> cashCollection = criteriaQuery.from(CashCollection.class);
		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();	
		if (deliveryBoyId!= null) {
			Join<CashCollection, DeliveryBoy> deliveryBoy = cashCollection.join("deliveryBoy", JoinType.INNER);
			predicates.add(criteriaBuilder.equal(deliveryBoy.get("id"), deliveryBoyId));
		}
		if (createdAt != null) {
			predicates.add(criteriaBuilder.equal(cashCollection.get("createdAt").as(Date.class),createdAt));
		}
		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(criteriaBuilder.count(cashCollection)).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));

		TypedQuery<Long> query = entityManager.createQuery(criteriaQuery);
		return query.getSingleResult();
	}
	
	



}
