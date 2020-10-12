package com.nice.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.VendorHistory;

@Repository
public interface VendorHistoryRepository extends JpaRepository<VendorHistory, Long> {

}
