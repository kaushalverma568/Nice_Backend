/**
 *
 */
package com.nice.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Set;

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

import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Repository;

import com.nice.model.City;
import com.nice.model.State;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Repository(value = "cityCustomRepository")
public class CityCustomRepositoryImpl implements CityCustomRepository {

	/**
	 *
	 */
	private static final String STATE_PARAM = "state";
	/**
	 *
	 */
	private static final String NAME_ARABIC = "nameArabic";

	/**
	 *
	 */
	private static final String NAME_ENGLISH = "nameEnglish";

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public List<City> getCityListBasedOnParams(final Integer startIndex, final Integer pageSize, final Boolean activeRecords, final Long stateId,
			final String searchKeyword, final Set<Long> idsIn, final Boolean isDefault) {
		Locale locale = LocaleContextHolder.getLocale();
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<City> criteriaQuery = criteriaBuilder.createQuery(City.class);
		/**
		 * Create and add a query root corresponding to the city.It is similar to the FROM clause in a JPQL query.
		 */
		Root<City> city = criteriaQuery.from(City.class);
		/**
		 * Inner Join to the other tables we can filter on like state.
		 */
		Join<City, State> state = city.join(STATE_PARAM, JoinType.INNER);

		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();
		if (activeRecords != null) {
			predicates.add(criteriaBuilder.equal(city.get("active"), activeRecords));
		}

		if (stateId != null) {
			predicates.add(criteriaBuilder.equal(state.get("id"), stateId));
		}

		if (searchKeyword != null) {
			Predicate predicateForNameEnglish = criteriaBuilder.like(criteriaBuilder.lower(city.get(NAME_ENGLISH)), "%" + searchKeyword.toLowerCase() + "%");
			Predicate predicateForNameArabic = criteriaBuilder.like(criteriaBuilder.lower(city.get(NAME_ARABIC)), "%" + searchKeyword.toLowerCase() + "%");
			predicates.add(criteriaBuilder.or(predicateForNameEnglish, predicateForNameArabic));
		}

		if (idsIn != null && !idsIn.isEmpty()) {
			predicates.add(criteriaBuilder.in(city.get("id")).value(idsIn));
		}
		if (isDefault != null) {
			predicates.add(criteriaBuilder.equal(city.get("isDefault"), isDefault));
		}
		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(city).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));
		/**
		 * order by name
		 */
		if (locale.getLanguage().equals("en")) {
			criteriaQuery.orderBy(criteriaBuilder.asc(city.get(NAME_ENGLISH)));
		} else {
			criteriaQuery.orderBy(criteriaBuilder.asc(city.get(NAME_ARABIC)));
		}
		/**
		 * Reducing multiple queries into single queries using graph </br>
		 * It allows defining a template by grouping the related persistence fields which we want to retrieve and lets us choose
		 * the graph type at runtime.
		 */
		EntityGraph<City> fetchGraph = entityManager.createEntityGraph(City.class);
		fetchGraph.addSubgraph(STATE_PARAM);
		TypedQuery<City> query = entityManager.createQuery(criteriaQuery).setHint("javax.persistence.loadgraph", fetchGraph);
		if (startIndex != null && pageSize != null) {
			query.setFirstResult(startIndex);
			query.setMaxResults(pageSize);
		}
		return query.getResultList();
	}

	@Override
	public Long getCityCountBasedOnParams(final Boolean activeRecords, final Long stateId, final String searchKeyword, final Set<Long> idsIn,
			final Boolean isDefault) {
		/**
		 * Create Criteria builder instance using entity manager
		 */
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		/**
		 * Create Criteria query object whichever object you want to return.
		 */
		CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
		/**
		 * Create and add a query root corresponding to the city.It is similar to the FROM clause in a JPQL query.
		 */
		Root<City> city = criteriaQuery.from(City.class);
		/**
		 * Inner Join to the other tables we can filter on like state.
		 */
		Join<City, State> state = city.join(STATE_PARAM, JoinType.INNER);

		/**
		 * Create the standard restrictions (i.e. the standard where clauses).
		 */
		List<Predicate> predicates = new ArrayList<>();
		if (activeRecords != null) {
			predicates.add(criteriaBuilder.equal(city.get("active"), activeRecords));
		}

		if (stateId != null) {
			predicates.add(criteriaBuilder.equal(state.get("id"), stateId));
		}

		if (searchKeyword != null) {
			Predicate predicateForNameEnglish = criteriaBuilder.like(criteriaBuilder.lower(city.get(NAME_ENGLISH)), "%" + searchKeyword.toLowerCase() + "%");
			Predicate predicateForNameArabic = criteriaBuilder.like(criteriaBuilder.lower(city.get(NAME_ARABIC)), "%" + searchKeyword.toLowerCase() + "%");
			predicates.add(criteriaBuilder.or(predicateForNameEnglish, predicateForNameArabic));
		}
		if (idsIn != null && !idsIn.isEmpty()) {
			predicates.add(criteriaBuilder.in(city.get("id")).value(idsIn));
		}
		if (isDefault != null) {
			predicates.add(criteriaBuilder.equal(city.get("isDefault"), isDefault));
		}
		/**
		 * Add the clauses for the query.
		 */
		criteriaQuery.select(criteriaBuilder.count(city)).where(criteriaBuilder.and(predicates.toArray(new Predicate[predicates.size()])));

		TypedQuery<Long> query = entityManager.createQuery(criteriaQuery);
		return query.getSingleResult();
	}

}
