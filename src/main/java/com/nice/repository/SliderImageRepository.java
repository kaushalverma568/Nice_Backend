package com.nice.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.SliderImage;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 26-Jun-2020
 */
@Repository
public interface SliderImageRepository extends JpaRepository<SliderImage, Long> {

	/**
	 * @param imageType
	 * @return
	 */
	List<SliderImage> findAllByType(String imageType);

	/**
	 * @return
	 */
	List<SliderImage> findAllByOrderByIdAsc();

}
