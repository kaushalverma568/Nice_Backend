/**
 *
 */
package com.nice.service.impl;

import java.util.Optional;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.nice.constant.HtmlSections;
import com.nice.dto.HtmlSectionDTO;
import com.nice.exception.NotFoundException;
import com.nice.exception.ValidationException;
import com.nice.locale.MessageByLocaleService;
import com.nice.mapper.HtmlSectionMapper;
import com.nice.model.HtmlSection;
import com.nice.repository.HtmlSectionRepository;
import com.nice.service.HtmlSectionService;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 26-Jun-2020
 */
@Service("sectionService")
@Transactional(rollbackFor = Throwable.class)
public class HtmlSectionServiceImpl implements HtmlSectionService {

	private static final Logger LOGGER = LoggerFactory.getLogger(HtmlSectionServiceImpl.class);

	@Autowired
	private HtmlSectionRepository sectionRepository;

	@Autowired
	private MessageByLocaleService messageByLocaleService;

	@Autowired
	private HtmlSectionMapper sectionMapper;

	@Override
	public HtmlSection addText(final HtmlSectionDTO sectionDTO) throws ValidationException {
		if (HtmlSections.getByValue(sectionDTO.getSectionType()) == null) {
			throw new ValidationException(messageByLocaleService.getMessage("sectionType.not.proper", null));
		}
		return sectionRepository.save(sectionMapper.toEntity(sectionDTO));
	}

	@Override
	public HtmlSection updateText(final HtmlSectionDTO sectionDTO) throws NotFoundException, ValidationException {
		if (HtmlSections.getByValue(sectionDTO.getSectionType()) == null) {
			throw new ValidationException(messageByLocaleService.getMessage("sectionType.not.present", new Object[] { sectionDTO.getSectionType() }));
		}
		if (sectionDTO.getId() == null) {
			throw new ValidationException(messageByLocaleService.getMessage("section.id.required", null));
		}

		Optional<HtmlSection> optExistingSection = sectionRepository.findById(sectionDTO.getId());
		if (!optExistingSection.isPresent()) {
			LOGGER.error("Section is not exists for SectionId {} ", sectionDTO.getId());
			throw new NotFoundException(messageByLocaleService.getMessage("section.not.found.id", new Object[] { sectionDTO.getId() }));
		}

		return sectionRepository.save(sectionMapper.toEntity(sectionDTO));
	}

	@Override
	public HtmlSectionDTO getText(final String type) throws NotFoundException {
		Optional<HtmlSection> section = sectionRepository.findBySectionType(type);
		if (!section.isPresent()) {
			LOGGER.error("Section is not exists for type {} ", type);
			throw new NotFoundException(messageByLocaleService.getMessage("section.not.found.type", new Object[] { type }));
		}
		HtmlSection result = section.get();
		return sectionMapper.toDto(result);
	}

	@Override
	public boolean isExists(final HtmlSectionDTO sectionDto) {
		/**
		 * is one Section already available or not
		 */

		if (sectionDto.getId() != null) {
			/**
			 * At the time of update is sectionValue is exist or not
			 */
			return sectionRepository.findBySectionTypeIgnoreCaseAndIdNot(sectionDto.getSectionType(), sectionDto.getId()).isPresent();
		} else {
			/**
			 * At the time of create is Section with same value exist or not
			 */
			return sectionRepository.findBySectionTypeIgnoreCase(sectionDto.getSectionType()).isPresent();

		}
	}
}