/**
 *
 */
package com.nice.repository;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

import org.springframework.stereotype.Repository;

import com.nice.dto.ProductParamRequestDTO;
import com.nice.model.Product;
import com.nice.util.CommonUtility;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 29-Jun-2020
 */
@Repository(value = "niceProductCustomRepository")
public class ProductCustomRepositoryImpl implements ProductCustomRepository {


	private static final String PRODUCT_TABLE_NAME = "product";
	private static final String PRODUCT_VARIANT_TABLE_NAME = "product_variant";
	@PersistenceContext
	private EntityManager entityManager;

	@SuppressWarnings("unchecked")
	@Override
	public List<Product> getProductListBasedOnParams(final ProductParamRequestDTO productParamRequestDTO, final Integer startIndex, final Integer pageSize) {
		Map<String, Object> paramMap = new HashMap<>();
		StringBuilder sqlQuery = new StringBuilder("select p.* from ").append(PRODUCT_TABLE_NAME).append(" p left join ").append(PRODUCT_VARIANT_TABLE_NAME)
				.append(" pv on p.id=pv.product_id where 1=1 ");

		addConditions(productParamRequestDTO, sqlQuery, paramMap);

		sqlQuery.append(" group by (p.id)");

		if (startIndex != null && pageSize != null) {
			sqlQuery.append(" offset :startIndex  limit :pageSize ");
			paramMap.put("startIndex", startIndex);
			paramMap.put("pageSize", pageSize);

		}

		Query q = entityManager.createNativeQuery(sqlQuery.toString(), "ProductMapping");
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return q.getResultList();
	}

	@Override
	public Long getProductCountBasedOnParams(final ProductParamRequestDTO productParamRequestDTO) {
		Map<String, Object> paramMap = new HashMap<>();
		/**
		 * Param For admin/customer/vendor
		 */

		StringBuilder sqlQuery = new StringBuilder(" select count(total)as count1 from (select p.id as total ,p.* from ").append(PRODUCT_TABLE_NAME)
				.append(" p left join ").append(PRODUCT_VARIANT_TABLE_NAME).append(" pv on p.id=pv.product_id where 1=1 ");
		addConditions(productParamRequestDTO, sqlQuery, paramMap);
		sqlQuery.append(" group by(p.id)) as abc");

		Query q = entityManager.createNativeQuery(sqlQuery.toString());
		paramMap.entrySet().forEach(p -> q.setParameter(p.getKey(), p.getValue()));
		return ((BigInteger) q.getSingleResult()).longValue();
	}

	/**
	 * @param productParamRequestDTO
	 * @param sqlQuery
	 * @param paramMap
	 * @return
	 */
	private StringBuilder addConditions(final ProductParamRequestDTO productParamRequestDTO, final StringBuilder sqlQuery, final Map<String, Object> paramMap) {
		if (productParamRequestDTO.getActiveRecords() != null) {
			sqlQuery.append(" and p.active = :activeRecords ");
			paramMap.put("activeRecords", productParamRequestDTO.getActiveRecords());
		}
		if (productParamRequestDTO.getProductVariantActiveRecords() != null) {
			sqlQuery.append(" and pv.active = :productVariantActiveRecords ");
			paramMap.put("productVariantActiveRecords", productParamRequestDTO.getProductVariantActiveRecords());
		}

		if (productParamRequestDTO.getVendorId() != null) {
			sqlQuery.append(" and p.vendor_id = :vendorId ");
			paramMap.put("vendorId", productParamRequestDTO.getVendorId());
		}
		
		if (productParamRequestDTO.getDiscountId() != null) {
			sqlQuery.append(" and p.discount_id = :discountId ");
			paramMap.put("discountId", productParamRequestDTO.getDiscountId());
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productParamRequestDTO.getCategoryIds())) {
			sqlQuery.append(
					" and p.category_id in (" + productParamRequestDTO.getCategoryIds().stream().map(String::valueOf).collect(Collectors.joining(",")) + " ) ");
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productParamRequestDTO.getSubcategoryIds())) {
			sqlQuery.append(" and p.subcategory_id in ("
					+ productParamRequestDTO.getSubcategoryIds().stream().map(String::valueOf).collect(Collectors.joining(",")) + " ) ");
		}

		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productParamRequestDTO.getBrandIds())) {
			sqlQuery.append(
					" and p.brand_id in (" + productParamRequestDTO.getBrandIds().stream().map(String::valueOf).collect(Collectors.joining(",")) + " ) ");
		}
		
		if (CommonUtility.NOT_NULL_NOT_EMPTY_LIST.test(productParamRequestDTO.getCuisineIds())) {
			sqlQuery.append(
					" and p.cuisine_id in (" + productParamRequestDTO.getCuisineIds().stream().map(String::valueOf).collect(Collectors.joining(",")) + " ) ");
		}

		if (productParamRequestDTO.getSearchKeyword() != null) {
			sqlQuery.append(" and lower(p.name) like CONCAT('%', :searchKeyword, '%')");
			paramMap.put("searchKeyword", productParamRequestDTO.getSearchKeyword().toLowerCase());
		}

		return sqlQuery;
	}


}
