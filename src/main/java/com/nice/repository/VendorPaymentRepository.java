package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.nice.model.VendorPayment;

/**
 *
 * @author : Kody Technolab PVT. LTD.
 * @date : 05-Sep-2020
 */
@Repository
public interface VendorPaymentRepository extends JpaRepository<VendorPayment, Long> {
	/**
	 * find vendor payment by vendorOrderId
	 *
	 * @param vendorOrderId
	 * @return
	 */
	Optional<VendorPayment> findByVendorOrderId(String vendorOrderId);

	/**
	 * find vendor by vendorOrderId and status
	 *
	 * @param vendorOrderId
	 * @param status
	 * @return
	 */
	Optional<VendorPayment> findByVendorOrderIdAndStatus(String vendorOrderId, String status);

	/**
	 * get list of payment by vendor id and status
	 *
	 * @param vendorId
	 * @param status
	 * @return
	 */
	List<VendorPayment> findAllByVendorIdAndStatus(Long vendorId, String status);

	@Query(value = "select vp.* from vendor_payment vp where vp.vendor_id=:vendorId and vp.subscription_plan_id=:subscriptionPlanId order by vp.created_at desc limit 1", nativeQuery = true)
	VendorPayment getLatestVendorPaymentByVendorIdAndSubscriptionPlan(Long vendorId, Long subscriptionPlanId);

}
