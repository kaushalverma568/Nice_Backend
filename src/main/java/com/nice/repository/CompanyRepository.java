/**
 *
 */
package com.nice.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Company;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Repository
public interface CompanyRepository extends JpaRepository<Company, Long> {

}
