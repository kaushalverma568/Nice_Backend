package com.nice.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;

import com.nice.dto.StateDTO;
import com.nice.dto.StateResponseDTO;
import com.nice.model.State;

/**
 * @author : Kody Technolab PVT. LTD.
 * @date   : 22-Jun-2020
 */
@Component
public class StateMapper {

	public StateResponseDTO toDto(final State state) {
		StateResponseDTO stateResponseDTO = new StateResponseDTO();
		BeanUtils.copyProperties(state, stateResponseDTO);
		stateResponseDTO.setCountryId(state.getCountry().getId());
		stateResponseDTO.setCountryName(state.getCountry().getName());
		return stateResponseDTO;
	}

	public State toEntity(final StateDTO stateDTO) {
		State state = new State();
		BeanUtils.copyProperties(stateDTO, state);
		return state;
	}

	public List<StateResponseDTO> toDtos(final List<State> stateList) {
		List<StateResponseDTO> results = new ArrayList<>();
		for (State state : stateList) {
			results.add(toDto(state));
		}
		return results;
	}
}
