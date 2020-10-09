package com.nice.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.nice.model.Area;
import com.nice.model.City;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : Oct 9, 2020
 */
@Repository(value = "areaRepository")
public interface AreaRepository extends JpaRepository<Area, Long> {

	/**
	 * Get area page by active
	 *
	 * @param  activeRecords
	 * @param  pageable
	 * @return
	 */
	Page<Area> findAllByActive(Boolean activeRecords, Pageable pageable);

	/**
	 * Get page of area by active and (english name or arabic name) contains search keyword
	 *
	 * @param  activeRecords
	 * @param  searchKeyWord
	 * @param  activeRecords2
	 * @param  searchKeyWord2
	 * @param  pageable
	 * @return
	 */
	Page<Area> findAllByActiveAndNameEnglishContainingIgnoreCaseOrActiveAndNameArabicContainingIgnoreCase(Boolean activeRecords, String searchKeyWord,
			Boolean activeRecords2, String searchKeyWord2, Pageable pageable);

	/**
	 * Get page of area by english name or arabic name contains search keyword
	 *
	 * @param  searchKeyWord
	 * @param  searchKeyWord2
	 * @param  pageable
	 * @return
	 */
	Page<Area> findAllByNameEnglishContainingIgnoreCaseOrNameArabicContainingIgnoreCase(String searchKeyWord, String searchKeyWord2, Pageable pageable);

	/**
	 * Get Area by area with same name(English) and not same Id
	 *
	 * @param  nameEnglish
	 * @param  id
	 * @return
	 */
	Optional<Area> findByNameEnglishIgnoreCaseAndCityAndIdNot(String nameEnglish, City city, Long id);

	/**
	 * Get Area by area with same name(English)
	 *
	 * @param  nameEnglish
	 * @return
	 */
	Optional<Area> findByNameEnglishIgnoreCaseAndCity(String nameEnglish, City city);

	/**
	 * Get Area by area with same name(Arabic) and not same Id
	 *
	 * @param  nameArabic
	 * @param  id
	 * @return
	 */
	Optional<Area> findByNameArabicIgnoreCaseAndCityAndIdNot(String nameArabic, City city, Long id);

	/**
	 * Get Area by area with same name(Arabic)
	 *
	 * @param  nameArabic
	 * @return
	 */
	Optional<Area> findByNameArabicIgnoreCaseAndCity(String nameArabic, City city);

	/**
	 * Get area by name english or arabice if exist
	 *
	 * @param  areaNameEnglish
	 * @param  areaNameArabic
	 * @return
	 */
	Optional<Area> findByNameEnglishIgnoreCaseOrNameArabicIgnoreCase(String areaNameEnglish, String areaNameArabic);

	Page<Area> findAllByCity(City city, Pageable pageable);

	Page<Area> findAllByCityAndNameEnglishContainingIgnoreCaseOrCityAndNameArabicContainingIgnoreCase(City city, String searchKeyWord, City city2,
			String searchKeyWord2, Pageable pageable);

	Page<Area> findAllByActiveAndCity(Boolean activeRecords, City city, Pageable pageable);

	Page<Area> findAllByActiveAndCityAndNameEnglishContainingIgnoreCaseOrActiveAndCityAndNameArabicContainingIgnoreCase(Boolean activeRecords, City city,
			String searchKeyWord, Boolean activeRecords2, City city2, String searchKeyWord2, Pageable pageable);

	List<Area> findAllByActive(Boolean activeRecords);

	List<Area> findAllByCity(City city);

	List<Area> findAllByActiveAndCity(Boolean activeRecords, City city);
}
