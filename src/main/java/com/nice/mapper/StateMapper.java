package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Component;

import com.nice.dto.StateDTO;
import com.nice.dto.StateResponseDTO;
import com.nice.model.State;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date : 22-Jun-2020
 */
@Component
public class StateMapper {

	public State toEntity(final StateDTO stateDTO) {
		State state = new State();
		BeanUtils.copyProperties(stateDTO, state);
		return state;
	}

	public StateResponseDTO toDto(final State state) {
		Locale locale = LocaleContextHolder.getLocale();
		StateResponseDTO stateResponseDTO = new StateResponseDTO();
		BeanUtils.copyProperties(state, stateResponseDTO);
		if (locale.getLanguage().equals("en")) {
			stateResponseDTO.setName(state.getNameEnglish());
			stateResponseDTO.setCountryName(state.getCountry().getNameEnglish());
		} else {
			stateResponseDTO.setName(state.getNameArabic());
			stateResponseDTO.setCountryName(state.getCountry().getNameArabic());
		}
		stateResponseDTO.setCountryId(state.getCountry().getId());
		return stateResponseDTO;
	}

	public List<StateResponseDTO> toDtos(final List<State> stateList) {
		List<StateResponseDTO> results = new ArrayList<>();
		for (State state : stateList) {
			results.add(toDto(state));
		}
		return results;
	}
}
