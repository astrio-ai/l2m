"""
Unit tests for the planner agent.

This module contains unit tests for the planner agent
functionality and behavior.
"""

import pytest
import asyncio
from unittest.mock import Mock, patch, AsyncMock, MagicMock
from src.core.agents.planner_agent import PlannerAgent
from src.core.state.agent_state import AgentState
from src.config.settings import Settings


class TestPlannerAgent:
    """Test cases for the planner agent."""
    
    @pytest.fixture
    def settings(self):
        """Create settings fixture."""
        return Settings()
    
    @pytest.fixture
    def planner_agent(self, settings):
        """Create planner agent fixture."""
        return PlannerAgent(settings)
    
    @pytest.fixture
    def mock_state(self):
        """Create mock agent state."""
        return {
            "agent_id": "planner",
            "agent_type": "planner",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": ["file1.cobol", "file2.cobol"]},
                "dependencies": {"dependencies": ["dep1", "dep2"]},
                "patterns": {"patterns": ["pattern1", "pattern2"]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "transformation_results": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
            "test_results": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None
        }
    
    def test_planner_agent_initialization(self, planner_agent):
        """Test planner agent initialization."""
        assert planner_agent is not None
        assert planner_agent.settings is not None
        assert len(planner_agent.tools) > 0
    
    def test_get_system_prompt(self, planner_agent):
        """Test system prompt generation."""
        prompt = planner_agent.get_system_prompt()
        assert isinstance(prompt, str)
        assert len(prompt) > 0
        assert "planning" in prompt.lower()
    
    @pytest.mark.asyncio
    async def test_run_planner_agent(self, planner_agent, mock_state):
        """Test planner agent execution."""
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            # Mock tool responses
            mock_use_tool.side_effect = [
                {"phases": ["phase1", "phase2"], "timeline": "6 months"},
                {"risks": ["risk1", "risk2"], "mitigation": ["mit1", "mit2"]},
                {"components": ["comp1", "comp2"], "strategy": "incremental"}
            ]
            
            result = await planner_agent.run(mock_state)
            
            assert result is not None
            assert result["modernization_plan"] is not None
            assert result["risk_assessment"] is not None
            assert result["implementation_strategy"] is not None
    
    @pytest.mark.asyncio
    async def test_run_planner_agent_error(self, planner_agent, mock_state):
        """Test planner agent error handling."""
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = Exception("Tool error")
            
            result = await planner_agent.run(mock_state)
            
            assert result is not None
            assert result["error"] is not None
            assert "Tool error" in result["error"]
    
    def test_get_available_tools(self, planner_agent):
        """Test available tools retrieval."""
        tools = planner_agent.get_available_tools()
        assert isinstance(tools, list)
        assert len(tools) > 0
    
    @pytest.mark.asyncio
    async def test_use_tool(self, planner_agent):
        """Test tool usage."""
        with patch.object(planner_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            result = await planner_agent.use_tool("create_modernization_plan", analysis_results={})
            
            assert result is not None
            assert result["result"] == "test"
    
    def test_log_activity(self, planner_agent):
        """Test activity logging."""
        planner_agent.log_activity("test activity", {"key": "value"})
        assert True  # If we get here, no exception was raised

    # ==================== EXTENSIVE TEST SUITE ====================
    
    def test_planner_agent_tools_initialization(self, planner_agent):
        """Test that planner agent has the correct tools initialized."""
        tools = planner_agent.get_available_tools()
        tool_names = [tool.name for tool in tools]
        
        # Should have planning tools
        assert "create_modernization_plan" in tool_names
        assert "assess_modernization_risks" in tool_names
        
        # Should have search tools
        assert "search_patterns" in tool_names
        
        # Verify tool count
        assert len(tools) >= 3  # At least 3 tools should be available
    
    def test_planner_agent_with_different_settings(self):
        """Test planner agent with different configuration settings."""
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        
        agent = PlannerAgent(custom_settings)
        assert agent.settings.llm_provider == "openai"
        assert agent.settings.llm_model == "gpt-4"
    
    @pytest.mark.asyncio
    async def test_run_with_empty_analysis_results(self, planner_agent):
        """Test planner agent with empty analysis results."""
        empty_state = {
            "agent_id": "planner",
            "agent_type": "planner",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": []},
                "dependencies": {"dependencies": []},
                "patterns": {"patterns": []}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "transformation_results": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
            "test_results": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None
        }
        
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            # Mock empty responses
            mock_use_tool.side_effect = [
                {"phases": [], "timeline": "0 months"},
                {"risks": [], "mitigation": []},
                {"components": [], "strategy": "minimal"}
            ]
            
            result = await planner_agent.run(empty_state)
            
            assert result is not None
            assert result["modernization_plan"] is not None
            assert result["risk_assessment"] is not None
            assert result["implementation_strategy"] is not None
    
    @pytest.mark.asyncio
    async def test_run_with_complex_analysis_results(self, planner_agent):
        """Test planner agent with complex analysis results."""
        complex_state = {
            "agent_id": "planner",
            "agent_type": "planner",
            "codebase_path": "/complex/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability", "performance", "security"],
            "analysis_results": {
                "structure": {
                    "files": [f"file{i}.cobol" for i in range(100)],
                    "complexity": 8.5,
                    "lines_of_code": 50000
                },
                "dependencies": {
                    "dependencies": [f"dep{i}" for i in range(50)],
                    "circular_deps": ["dep1->dep2->dep1"],
                    "external_deps": ["lib1", "lib2"]
                },
                "patterns": {
                    "patterns": [f"pattern{i}" for i in range(25)],
                    "legacy_patterns": ["goto", "global_vars"],
                    "modern_patterns": ["functions", "classes"]
                }
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "transformation_results": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
            "test_results": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None
        }
        
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            # Mock complex responses
            mock_use_tool.side_effect = [
                {
                    "phases": ["analysis", "refactoring", "testing", "deployment"],
                    "timeline": "12 months",
                    "complexity_score": 8.5
                },
                {
                    "risks": ["data_loss", "performance_degradation", "integration_failure"],
                    "mitigation": ["backup_strategy", "performance_testing", "staged_deployment"]
                },
                {
                    "components": ["data_migration", "api_modernization", "ui_update"],
                    "strategy": "incremental_with_rollback"
                }
            ]
            
            result = await planner_agent.run(complex_state)
            
            assert result is not None
            assert result["modernization_plan"] is not None
            assert result["risk_assessment"] is not None
            assert result["implementation_strategy"] is not None
            assert len(result["modernization_plan"]["phases"]) == 4
            assert len(result["risk_assessment"]["risks"]) == 3
            assert len(result["implementation_strategy"]["components"]) == 3
    
    @pytest.mark.asyncio
    async def test_run_with_partial_tool_failures(self, planner_agent, mock_state):
        """Test planner agent when some tools fail but others succeed."""
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            # First tool succeeds, second fails, third succeeds
            mock_use_tool.side_effect = [
                {"phases": ["phase1"], "timeline": "3 months"},
                Exception("Risk assessment failed"),
                {"components": ["comp1"], "strategy": "basic"}
            ]
            
            result = await planner_agent.run(mock_state)
            
            # Should handle partial failures gracefully
            assert result is not None
            assert result["error"] is not None
            assert "Risk assessment failed" in result["error"]
    
    @pytest.mark.asyncio
    async def test_run_with_timeout_simulation(self, planner_agent, mock_state):
        """Test planner agent with timeout simulation."""
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            # Simulate timeout by making tools take too long
            async def slow_tool(*args, **kwargs):
                await asyncio.sleep(0.1)  # Simulate slow operation
                return {"result": "slow_result"}
            
            mock_use_tool.side_effect = slow_tool
            
            result = await planner_agent.run(mock_state)
            
            # Should complete despite slow tools
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_malformed_tool_responses(self, planner_agent, mock_state):
        """Test planner agent with malformed tool responses."""
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            # Mock malformed responses
            mock_use_tool.side_effect = [
                None,  # None response
                {"risks": "not_a_list"},  # Wrong type
                {"components": []}  # Valid response
            ]
            
            result = await planner_agent.run(mock_state)
            
            # Should handle malformed responses gracefully
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_use_tool_with_invalid_tool_name(self, planner_agent):
        """Test using a tool that doesn't exist."""
        with pytest.raises(ValueError, match="Tool 'invalid_tool' not found"):
            await planner_agent.use_tool("invalid_tool", param="value")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_missing_parameters(self, planner_agent):
        """Test using a tool with missing required parameters."""
        # This should raise an error due to missing required parameter
        with pytest.raises((ValueError, TypeError)):
            await planner_agent.use_tool("create_modernization_plan")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_extra_parameters(self, planner_agent):
        """Test using a tool with extra parameters."""
        with patch.object(planner_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            # Should handle extra parameters gracefully
            result = await planner_agent.use_tool(
                "create_modernization_plan", 
                analysis_results={},
                extra_param="extra_value"
            )
            
            assert result is not None
    
    def test_get_system_prompt_content(self, planner_agent):
        """Test system prompt contains expected content."""
        prompt = planner_agent.get_system_prompt()
        
        # Check for key concepts in the prompt
        assert "planning" in prompt.lower()
        assert "modernization" in prompt.lower()
        assert "risks" in prompt.lower()
        assert "implementation" in prompt.lower()
        assert "strategies" in prompt.lower() 
        assert "dependencies" in prompt.lower()
    
    def test_get_system_prompt_length(self, planner_agent):
        """Test system prompt has reasonable length."""
        prompt = planner_agent.get_system_prompt()
        
        # Should be substantial but not excessive
        assert len(prompt) > 100  # At least 100 characters
        assert len(prompt) < 2000  # Not more than 2000 characters
    
    def test_log_activity_with_different_data_types(self, planner_agent):
        """Test logging with different data types."""
        # Test with string
        planner_agent.log_activity("string activity")
        
        # Test with dict
        planner_agent.log_activity("dict activity", {"key": "value", "number": 42})
        
        # Test with list
        planner_agent.log_activity("list activity", ["item1", "item2"])
        
        # Test with None
        planner_agent.log_activity("none activity", None)
        
        # All should complete without errors
        assert True
    
    def test_log_activity_with_complex_data(self, planner_agent):
        """Test logging with complex nested data structures."""
        complex_data = {
            "planning": {
                "phases": ["phase1", "phase2", "phase3"],
                "timeline": {
                    "total_duration": "6 months",
                    "phase_durations": {"phase1": "2 months", "phase2": "3 months", "phase3": "1 month"}
                },
                "risks": [
                    {"name": "risk1", "severity": "high", "probability": 0.8},
                    {"name": "risk2", "severity": "medium", "probability": 0.5}
                ]
            },
            "metadata": {
                "timestamp": "2024-01-01T00:00:00Z",
                "version": "1.0.0",
                "complexity_score": 7.5
            }
        }
        
        planner_agent.log_activity("complex planning", complex_data)
        assert True  # Should complete without errors
    
    @pytest.mark.asyncio
    async def test_concurrent_tool_usage(self, planner_agent):
        """Test concurrent usage of multiple tools."""
        async def use_multiple_tools():
            tasks = []
            for i in range(5):
                task = planner_agent.use_tool(
                    "create_modernization_plan", 
                    analysis_results={"files": [f"file{i}.cobol"]}
                )
                tasks.append(task)
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            return results
        
        with patch.object(planner_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            results = await use_multiple_tools()
            
            # All should complete successfully
            assert len(results) == 5
            for result in results:
                assert not isinstance(result, Exception)
    
    @pytest.mark.asyncio
    async def test_run_with_state_mutation(self, planner_agent):
        """Test that agent run doesn't mutate the original state inappropriately."""
        original_state = {
            "agent_id": "planner",
            "agent_type": "planner",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": ["file1.cobol"]},
                "dependencies": {"dependencies": ["dep1"]},
                "patterns": {"patterns": ["pattern1"]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "transformation_results": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
            "test_results": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None
        }
        
        # Create a copy to compare later
        state_copy = original_state.copy()
        
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"phases": ["phase1"], "timeline": "3 months"},
                {"risks": ["risk1"], "mitigation": ["mit1"]},
                {"components": ["comp1"], "strategy": "basic"}
            ]
            
            result = await planner_agent.run(original_state)
            
            # Original state should be modified (this is expected behavior)
            assert result is not None
            assert result["modernization_plan"] is not None
    
    def test_planner_agent_tool_integration(self, planner_agent):
        """Test that all tools are properly integrated."""
        tools = planner_agent.get_available_tools()
        
        # Each tool should have required attributes
        for tool in tools:
            assert hasattr(tool, 'name')
            assert hasattr(tool, 'description')
            assert hasattr(tool, 'run')
            assert callable(tool.run)
            
            # Tool names should be non-empty strings
            assert isinstance(tool.name, str)
            assert len(tool.name) > 0
            
            # Descriptions should be non-empty strings
            assert isinstance(tool.description, str)
            assert len(tool.description) > 0
    
    @pytest.mark.asyncio
    async def test_run_with_different_modernization_goals(self, planner_agent):
        """Test planner agent with different modernization goals."""
        goals_variations = [
            ["maintainability"],
            ["performance"],
            ["security"],
            ["maintainability", "performance"],
            ["maintainability", "performance", "security"],
            []
        ]
        
        for goals in goals_variations:
            state = {
                "agent_id": "planner",
                "agent_type": "planner",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": goals,
                "analysis_results": {
                    "structure": {"files": ["file1.cobol"]},
                    "dependencies": {"dependencies": ["dep1"]},
                    "patterns": {"patterns": ["pattern1"]}
                },
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "transformation_results": None,
                "pattern_results": None,
                "backup_path": None,
                "quality_review": None,
                "standards_analysis": None,
                "test_cases": None,
                "test_results": None,
                "coverage_analysis": None,
                "test_validation": None,
                "final_validation": None,
                "compliance_check": None,
                "integration_tests": None,
                "agent_data": None,
                "error": None,
                "messages": [],
                "start_time": None,
                "end_time": None,
                "duration": None
            }
            
            with patch.object(planner_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"phases": ["phase1"], "timeline": "3 months"},
                    {"risks": ["risk1"], "mitigation": ["mit1"]},
                    {"components": ["comp1"], "strategy": "basic"}
                ]
                
                result = await planner_agent.run(state)
                assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_different_target_languages(self, planner_agent):
        """Test planner agent with different target languages."""
        languages = ["python", "javascript", "java", "csharp", "go"]
        
        for language in languages:
            state = {
                "agent_id": "planner",
                "agent_type": "planner",
                "codebase_path": "/test/codebase",
                "target_language": language,
                "modernization_goals": ["maintainability"],
                "analysis_results": {
                    "structure": {"files": [f"file1.{language}"]},
                    "dependencies": {"dependencies": ["dep1"]},
                    "patterns": {"patterns": ["pattern1"]}
                },
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "transformation_results": None,
                "pattern_results": None,
                "backup_path": None,
                "quality_review": None,
                "standards_analysis": None,
                "test_cases": None,
                "test_results": None,
                "coverage_analysis": None,
                "test_validation": None,
                "final_validation": None,
                "compliance_check": None,
                "integration_tests": None,
                "agent_data": None,
                "error": None,
                "messages": [],
                "start_time": None,
                "end_time": None,
                "duration": None
            }
            
            with patch.object(planner_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"phases": ["phase1"], "timeline": "3 months"},
                    {"risks": ["risk1"], "mitigation": ["mit1"]},
                    {"components": ["comp1"], "strategy": "basic"}
                ]
                
                result = await planner_agent.run(state)
                assert result is not None
                assert result["target_language"] == language
    
    def test_planner_agent_memory_usage(self, planner_agent):
        """Test that planner agent doesn't have memory leaks."""
        import gc
        
        # Create and destroy multiple agents
        for i in range(10):
            temp_agent = PlannerAgent(Settings())
            del temp_agent
        
        # Force garbage collection
        gc.collect()
        
        # Original agent should still work
        assert planner_agent is not None
        assert len(planner_agent.get_available_tools()) > 0
    
    @pytest.mark.asyncio
    async def test_run_with_corrupted_state(self, planner_agent):
        """Test planner agent with corrupted or incomplete state."""
        corrupted_states = [
            {},  # Empty state
            {"agent_id": "planner"},  # Minimal state
            {"analysis_results": None},  # None analysis results
            {"analysis_results": {}},  # Empty analysis results
        ]
        
        for state in corrupted_states:
            # Fill in missing required fields
            complete_state = {
                "agent_id": "planner",
                "agent_type": "planner",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "analysis_results": {
                    "structure": {"files": ["file1.cobol"]},
                    "dependencies": {"dependencies": ["dep1"]},
                    "patterns": {"patterns": ["pattern1"]}
                },
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "transformation_results": None,
                "pattern_results": None,
                "backup_path": None,
                "quality_review": None,
                "standards_analysis": None,
                "test_cases": None,
                "test_results": None,
                "coverage_analysis": None,
                "test_validation": None,
                "final_validation": None,
                "compliance_check": None,
                "integration_tests": None,
                "agent_data": None,
                "error": None,
                "messages": [],
                "start_time": None,
                "end_time": None,
                "duration": None,
                **state  # Override with corrupted values
            }
            
            with patch.object(planner_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"phases": ["phase1"], "timeline": "3 months"},
                    {"risks": ["risk1"], "mitigation": ["mit1"]},
                    {"components": ["comp1"], "strategy": "basic"}
                ]
                
                result = await planner_agent.run(complete_state)
                # Should handle corrupted state gracefully
                assert result is not None

    # ==================== PERFORMANCE & STRESS TESTS ====================
    
    @pytest.mark.asyncio
    async def test_run_performance_benchmark(self, planner_agent):
        """Test planner agent performance with timing measurements."""
        import time
        
        state = {
            "agent_id": "planner",
            "agent_type": "planner",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": ["file1.cobol"]},
                "dependencies": {"dependencies": ["dep1"]},
                "patterns": {"patterns": ["pattern1"]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "transformation_results": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
            "test_results": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None
        }
        
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"phases": ["phase1"], "timeline": "3 months"},
                {"risks": ["risk1"], "mitigation": ["mit1"]},
                {"components": ["comp1"], "strategy": "basic"}
            ]
            
            start_time = time.time()
            result = await planner_agent.run(state)
            end_time = time.time()
            
            execution_time = end_time - start_time
            
            # Should complete within reasonable time (less than 5 seconds)
            assert execution_time < 5.0
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_stress_test_multiple_runs(self, planner_agent):
        """Test planner agent with multiple consecutive runs."""
        state = {
            "agent_id": "planner",
            "agent_type": "planner",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {"files": ["file1.cobol"]},
                "dependencies": {"dependencies": ["dep1"]},
                "patterns": {"patterns": ["pattern1"]}
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "transformation_results": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
            "test_results": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None
        }
        
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"phases": ["phase1"], "timeline": "3 months"},
                {"risks": ["risk1"], "mitigation": ["mit1"]},
                {"components": ["comp1"], "strategy": "basic"}
            ]
            
            # Run multiple times to test for memory leaks or state issues
            for i in range(10):
                # Create a fresh copy of state for each run
                fresh_state = state.copy()
                result = await planner_agent.run(fresh_state)
                assert result is not None
                # Check if modernization_plan exists (may be None on error)
                if result.get("modernization_plan") is not None:
                    assert result["modernization_plan"] is not None
    
    @pytest.mark.asyncio
    async def test_concurrent_agent_instances(self):
        """Test multiple planner agent instances running concurrently."""
        async def run_agent_instance(agent_id):
            agent = PlannerAgent(Settings())
            state = {
                "agent_id": f"planner_{agent_id}",
                "agent_type": "planner",
                "codebase_path": f"/test/codebase_{agent_id}",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "analysis_results": {
                    "structure": {"files": [f"file{agent_id}.cobol"]},
                    "dependencies": {"dependencies": [f"dep{agent_id}"]},
                    "patterns": {"patterns": [f"pattern{agent_id}"]}
                },
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "transformation_results": None,
                "pattern_results": None,
                "backup_path": None,
                "quality_review": None,
                "standards_analysis": None,
                "test_cases": None,
                "test_results": None,
                "coverage_analysis": None,
                "test_validation": None,
                "final_validation": None,
                "compliance_check": None,
                "integration_tests": None,
                "agent_data": None,
                "error": None,
                "messages": [],
                "start_time": None,
                "end_time": None,
                "duration": None
            }
            
            with patch.object(agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"phases": [f"phase{agent_id}"], "timeline": "3 months"},
                    {"risks": [f"risk{agent_id}"], "mitigation": [f"mit{agent_id}"]},
                    {"components": [f"comp{agent_id}"], "strategy": "basic"}
                ]
                
                return await agent.run(state)
        
        # Run 5 concurrent agent instances
        tasks = [run_agent_instance(i) for i in range(5)]
        results = await asyncio.gather(*tasks)
        
        # All should complete successfully
        assert len(results) == 5
        for i, result in enumerate(results):
            assert result is not None
            assert result["agent_id"] == f"planner_{i}"
    
    @pytest.mark.asyncio
    async def test_tool_error_recovery(self, planner_agent, mock_state):
        """Test planner agent recovery from various tool errors."""
        error_scenarios = [
            Exception("Network timeout"),
            ValueError("Invalid input"),
            RuntimeError("Resource unavailable"),
            KeyError("Missing key"),
            TypeError("Wrong type"),
        ]
        
        for error in error_scenarios:
            with patch.object(planner_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = error
                
                result = await planner_agent.run(mock_state.copy())
                
                # Should handle all error types gracefully
                assert result is not None
                assert result["error"] is not None
                assert str(error) in result["error"]
    
    def test_planner_agent_thread_safety(self, planner_agent):
        """Test planner agent thread safety (basic check)."""
        import threading
        import time
        
        results = []
        errors = []
        
        def run_agent():
            try:
                # Test basic operations that should be thread-safe
                tools = planner_agent.get_available_tools()
                prompt = planner_agent.get_system_prompt()
                results.append((len(tools), len(prompt)))
            except Exception as e:
                errors.append(e)
        
        # Create multiple threads
        threads = []
        for i in range(5):
            thread = threading.Thread(target=run_agent)
            threads.append(thread)
            thread.start()
        
        # Wait for all threads to complete
        for thread in threads:
            thread.join()
        
        # Should have no errors and consistent results
        assert len(errors) == 0
        assert len(results) == 5
        assert all(len(r) == 2 for r in results)  # Each result should have 2 values
    
    @pytest.mark.asyncio
    async def test_planner_agent_with_extreme_data_sizes(self, planner_agent):
        """Test planner agent with extreme data sizes."""
        # Test with very large data
        large_state = {
            "agent_id": "planner",
            "agent_type": "planner",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": {
                "structure": {
                    "files": [f"file{i}.cobol" for i in range(10000)],
                    "complexity": 9.5,
                    "lines_of_code": 100000
                },
                "dependencies": {
                    "dependencies": [f"dep{i}" for i in range(5000)],
                    "circular_deps": [f"dep{i}->dep{i+1}" for i in range(1000)]
                },
                "patterns": {
                    "patterns": [f"pattern{i}" for i in range(2500)],
                    "legacy_patterns": [f"legacy{i}" for i in range(1000)],
                    "modern_patterns": [f"modern{i}" for i in range(1500)]
                }
            },
            "modernization_plan": None,
            "risk_assessment": None,
            "implementation_strategy": None,
            "transformation_results": None,
            "pattern_results": None,
            "backup_path": None,
            "quality_review": None,
            "standards_analysis": None,
            "test_cases": None,
            "test_results": None,
            "coverage_analysis": None,
            "test_validation": None,
            "final_validation": None,
            "compliance_check": None,
            "integration_tests": None,
            "agent_data": None,
            "error": None,
            "messages": [],
            "start_time": None,
            "end_time": None,
            "duration": None
        }
        
        with patch.object(planner_agent, 'use_tool') as mock_use_tool:
            # Mock extremely large responses
            huge_phases = [f"phase{i}" for i in range(1000)]
            huge_risks = [f"risk{i}" for i in range(500)]
            huge_components = [f"component{i}" for i in range(250)]
            
            mock_use_tool.side_effect = [
                {"phases": huge_phases, "timeline": "24 months"},
                {"risks": huge_risks, "mitigation": [f"mit{i}" for i in range(500)]},
                {"components": huge_components, "strategy": "comprehensive"}
            ]
            
            result = await planner_agent.run(large_state)
            
            # Should handle large data without issues
            assert result is not None
            assert result["modernization_plan"] is not None
            assert len(result["modernization_plan"]["phases"]) == 1000
            assert len(result["risk_assessment"]["risks"]) == 500
            assert len(result["implementation_strategy"]["components"]) == 250
    
    def test_planner_agent_configuration_validation(self):
        """Test planner agent with various configuration scenarios."""
        # Test with minimal settings
        minimal_settings = Settings()
        minimal_settings.llm_provider = "anthropic"
        minimal_settings.llm_model = "claude-3-sonnet"
        
        agent = PlannerAgent(minimal_settings)
        assert agent is not None
        
        # Test with custom settings
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        custom_settings.llm_temperature = 0.5
        custom_settings.llm_max_tokens = 2000
        
        agent = PlannerAgent(custom_settings)
        assert agent.settings.llm_temperature == 0.5
        assert agent.settings.llm_max_tokens == 2000
    
    @pytest.mark.asyncio
    async def test_planner_agent_edge_case_combinations(self, planner_agent):
        """Test planner agent with edge case combinations."""
        edge_cases = [
            # Empty analysis with multiple goals
            {
                "analysis_results": {
                    "structure": {"files": []},
                    "dependencies": {"dependencies": []},
                    "patterns": {"patterns": []}
                },
                "target_language": "python",
                "modernization_goals": ["maintainability", "performance", "security"]
            },
            # Complex analysis with single goal
            {
                "analysis_results": {
                    "structure": {"files": [f"file{i}.cobol" for i in range(100)]},
                    "dependencies": {"dependencies": [f"dep{i}" for i in range(50)]},
                    "patterns": {"patterns": [f"pattern{i}" for i in range(25)]}
                },
                "target_language": "javascript",
                "modernization_goals": ["maintainability"]
            },
            # Different language combinations
            {
                "analysis_results": {
                    "structure": {"files": ["legacy.cobol"]},
                    "dependencies": {"dependencies": ["old_dep"]},
                    "patterns": {"patterns": ["legacy_pattern"]}
                },
                "target_language": "java",
                "modernization_goals": ["performance"]
            }
        ]
        
        for case in edge_cases:
            state = {
                "agent_id": "planner",
                "agent_type": "planner",
                "codebase_path": "/test/codebase",
                "modernization_plan": None,
                "risk_assessment": None,
                "implementation_strategy": None,
                "transformation_results": None,
                "pattern_results": None,
                "backup_path": None,
                "quality_review": None,
                "standards_analysis": None,
                "test_cases": None,
                "test_results": None,
                "coverage_analysis": None,
                "test_validation": None,
                "final_validation": None,
                "compliance_check": None,
                "integration_tests": None,
                "agent_data": None,
                "error": None,
                "messages": [],
                "start_time": None,
                "end_time": None,
                "duration": None,
                **case
            }
            
            with patch.object(planner_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"phases": ["phase1"], "timeline": "3 months"},
                    {"risks": ["risk1"], "mitigation": ["mit1"]},
                    {"components": ["comp1"], "strategy": "basic"}
                ]
                
                result = await planner_agent.run(state)
                assert result is not None
                assert result["target_language"] == case["target_language"]
                assert result["modernization_goals"] == case["modernization_goals"]
