/**
 *
 */
package com.nice.repository;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityGraph;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Repository;

import com.nice.dto.DeliveryBoyFilterDTO;
import com.nice.model.DeliveryBoy;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date   : Sep 2, 2020
 */
@Repository(value = "deliveryBoyCustomRepository")
public class DeliveryBoyCustomRepositoryImpl implements DeliveryBoyCustomRepository {

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<DeliveryBoy> getDeliveryBoyListBasedOnParams(final Integer startIndex, final Integer pageSize,
			final DeliveryBoyFilterDTO deliveryBoyFilterDTO) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<DeliveryBoy> criteriaQuery = criteriaBuilder.createQuery(DeliveryBoy.class);
		/**
		 * Create and add a query root corresponding to the deliveryBoy.It is similar to the FROM clause in a JPQL query.
		 */
		Root<DeliveryBoy> deliveryBoy = criteriaQuery.from(DeliveryBoy.class);
		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();

		if (deliveryBoyFilterDTO.getActiveRecords() != null) {
			predicates.add(criteriaBuilder.equal(deliveryBoy.get("active"), deliveryBoyFilterDTO.getActiveRecords()));
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(deliveryBoyFilterDTO.getSearchKeyword())) {
			Predicate predicateForFirstNameEnglish = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("firstNameEnglish")),
					"%" + deliveryBoyFilterDTO.getSearchKeyword().toLowerCase() + "%");
			Predicate predicateForFirstNameArabic = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("firstNameArabic")),
					"%" + deliveryBoyFilterDTO.getSearchKeyword().toLowerCase() + "%");
			Predicate predicateForLastNameEnglish = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("lastNameEnglish")),
					"%" + deliveryBoyFilterDTO.getSearchKeyword().toLowerCase() + "%");
			Predicate predicateForLastNameArabic = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("lastNameArabic")),
					"%" + deliveryBoyFilterDTO.getSearchKeyword().toLowerCase() + "%");

			predicates.add(
					criteriaBuilder.or(predicateForFirstNameEnglish, predicateForFirstNameArabic, predicateForLastNameEnglish, predicateForLastNameArabic));
		}

		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(deliveryBoy).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		/**
		 * add sorting
		 */
		if (Direction.fromString(deliveryBoyFilterDTO.getSortByDirection()).isAscending()) {
			criteriaQuery.orderBy(criteriaBuilder.asc(deliveryBoy.get(deliveryBoyFilterDTO.getSortByField())));
		} else {
			criteriaQuery.orderBy(criteriaBuilder.desc(deliveryBoy.get(deliveryBoyFilterDTO.getSortByField())));
		}
		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields which we want to retrieve and lets us choose the graph type at runtime.
		 */
		EntityGraph<DeliveryBoy> fetchGraph = entityManager.createEntityGraph(DeliveryBoy.class);
		TypedQuery<DeliveryBoy> query = entityManager.createQuery(criteriaQuery).setHint("javax.persistence.loadgraph", fetchGraph);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}
		return query.getResultList();
	}

	@Override
	public Long getDeliveryBoyCountBasedOnParams(final DeliveryBoyFilterDTO deliveryBoyFilterDTO) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
		/**
		 * Create and add a query root corresponding to the deliveryBoy.It is similar to the FROM clause in a JPQL query.
		 */
		Root<DeliveryBoy> deliveryBoy = criteriaQuery.from(DeliveryBoy.class);
		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();

		if (deliveryBoyFilterDTO.getActiveRecords() != null) {
			predicates.add(criteriaBuilder.equal(deliveryBoy.get("active"), deliveryBoyFilterDTO.getActiveRecords()));
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_NOT_BLANK_STRING.test(deliveryBoyFilterDTO.getSearchKeyword())) {
			Predicate predicateForFirstNameEnglish = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("firstNameEnglish")),
					"%" + deliveryBoyFilterDTO.getSearchKeyword().toLowerCase() + "%");
			Predicate predicateForFirstNameArabic = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("firstNameArabic")),
					"%" + deliveryBoyFilterDTO.getSearchKeyword().toLowerCase() + "%");
			Predicate predicateForLastNameEnglish = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("lastNameEnglish")),
					"%" + deliveryBoyFilterDTO.getSearchKeyword().toLowerCase() + "%");
			Predicate predicateForLastNameArabic = criteriaBuilder.like(criteriaBuilder.lower(deliveryBoy.get("lastNameArabic")),
					"%" + deliveryBoyFilterDTO.getSearchKeyword().toLowerCase() + "%");

			predicates.add(
					criteriaBuilder.or(predicateForFirstNameEnglish, predicateForFirstNameArabic, predicateForLastNameEnglish, predicateForLastNameArabic));
		}
		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(criteriaBuilder.count(deliveryBoy)).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));

		TypedQuery<Long> query = entityManager.createQuery(criteriaQuery);
		return query.getSingleResult();
	}
}
