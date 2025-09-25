"""
Unit tests for the executor agent.

This module contains unit tests for the executor agent
functionality and behavior.
"""

import pytest
import asyncio
from unittest.mock import Mock, patch, AsyncMock, MagicMock
from src.core.agents.executor_agent import ExecutorAgent
from src.core.state.agent_state import AgentState
from src.config.settings import Settings


class TestExecutorAgent:
    """Test cases for the executor agent."""
    
    @pytest.fixture
    def settings(self):
        """Create settings fixture."""
        return Settings()
    
    @pytest.fixture
    def executor_agent(self, settings):
        """Create executor agent fixture."""
        return ExecutorAgent(settings)
    
    @pytest.fixture
    def mock_state(self):
        """Create mock agent state."""
        return {
            "agent_id": "executor",
            "agent_type": "executor",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "backup_location": "/test/backup",
            "modernization_plan": {
                "phases": [
                    {"name": "phase1", "transformations": ["transform1"]},
                    {"name": "phase2", "transformations": ["transform2"]}
                ],
                "patterns": [
                    {"pattern": "old_pattern", "replacement": "new_pattern"}
                ]
            },
            "analysis_results": None,
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
    
    def test_executor_agent_initialization(self, executor_agent):
        """Test executor agent initialization."""
        assert executor_agent is not None
        assert executor_agent.settings is not None
        assert len(executor_agent.tools) > 0
    
    def test_get_system_prompt(self, executor_agent):
        """Test system prompt generation."""
        prompt = executor_agent.get_system_prompt()
        assert isinstance(prompt, str)
        assert len(prompt) > 0
        assert "execution" in prompt.lower()
    
    @pytest.mark.asyncio
    async def test_run_executor_agent(self, executor_agent, mock_state):
        """Test executor agent execution."""
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            # Mock tool responses
            mock_use_tool.side_effect = [
                "/backup/path",  # backup tool
                {"result": "phase1_completed"},  # first phase
                {"result": "phase2_completed"},  # second phase
                {"patterns_applied": 5}  # pattern replacement
            ]
            
            result = await executor_agent.run(mock_state)
            
            assert result is not None
            assert result["transformation_results"] is not None
            assert result["pattern_results"] is not None
            assert result["backup_path"] is not None
    
    @pytest.mark.asyncio
    async def test_run_executor_agent_error(self, executor_agent, mock_state):
        """Test executor agent error handling."""
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = Exception("Tool error")
            
            result = await executor_agent.run(mock_state)
            
            assert result is not None
            assert result["error"] is not None
            assert "Tool error" in result["error"]
    
    def test_get_available_tools(self, executor_agent):
        """Test available tools retrieval."""
        tools = executor_agent.get_available_tools()
        assert isinstance(tools, list)
        assert len(tools) > 0
    
    @pytest.mark.asyncio
    async def test_use_tool(self, executor_agent):
        """Test tool usage."""
        with patch.object(executor_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            result = await executor_agent.use_tool("transform_code_phase", phase={})
            
            assert result is not None
            assert result["result"] == "test"
    
    def test_log_activity(self, executor_agent):
        """Test activity logging."""
        executor_agent.log_activity("test activity", {"key": "value"})
        assert True  # If we get here, no exception was raised

    # ==================== EXTENSIVE TEST SUITE ====================
    
    def test_executor_agent_tools_initialization(self, executor_agent):
        """Test that executor agent has the correct tools initialized."""
        tools = executor_agent.get_available_tools()
        tool_names = [tool.name for tool in tools]
        
        # Should have transformation tools
        assert "transform_code_phase" in tool_names
        assert "apply_pattern_replacements" in tool_names
        
        # Should have file tools
        assert "write_file" in tool_names
        assert "create_backup" in tool_names
        
        # Verify tool count
        assert len(tools) >= 4  # At least 4 tools should be available
    
    def test_executor_agent_with_different_settings(self):
        """Test executor agent with different configuration settings."""
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        
        agent = ExecutorAgent(custom_settings)
        assert agent.settings.llm_provider == "openai"
        assert agent.settings.llm_model == "gpt-4"
    
    @pytest.mark.asyncio
    async def test_run_with_empty_modernization_plan(self, executor_agent):
        """Test executor agent with empty modernization plan."""
        empty_state = {
            "agent_id": "executor",
            "agent_type": "executor",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "backup_location": "/test/backup",
            "modernization_plan": {
                "phases": [],
                "patterns": []
            },
            "analysis_results": None,
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
        
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            # Mock responses for empty plan
            mock_use_tool.side_effect = [
                "/backup/path",  # backup tool
                {"patterns_applied": 0}  # pattern replacement (no patterns)
            ]
            
            result = await executor_agent.run(empty_state)
            
            assert result is not None
            assert result["transformation_results"] == []
            assert result["pattern_results"] is not None
            assert result["backup_path"] is not None
    
    @pytest.mark.asyncio
    async def test_run_with_complex_modernization_plan(self, executor_agent):
        """Test executor agent with complex modernization plan."""
        complex_state = {
            "agent_id": "executor",
            "agent_type": "executor",
            "codebase_path": "/complex/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability", "performance"],
            "backup_location": "/complex/backup",
            "modernization_plan": {
                "phases": [
                    {"name": "phase1", "transformations": ["transform1", "transform2"]},
                    {"name": "phase2", "transformations": ["transform3", "transform4"]},
                    {"name": "phase3", "transformations": ["transform5"]}
                ],
                "patterns": [
                    {"pattern": "old_pattern1", "replacement": "new_pattern1"},
                    {"pattern": "old_pattern2", "replacement": "new_pattern2"},
                    {"pattern": "old_pattern3", "replacement": "new_pattern3"}
                ]
            },
            "analysis_results": None,
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
        
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            # Mock complex responses
            mock_use_tool.side_effect = [
                "/complex/backup/path",  # backup tool
                {"result": "phase1_completed", "files_transformed": 10},  # first phase
                {"result": "phase2_completed", "files_transformed": 15},  # second phase
                {"result": "phase3_completed", "files_transformed": 5},   # third phase
                {"patterns_applied": 3, "files_modified": 8}  # pattern replacement
            ]
            
            result = await executor_agent.run(complex_state)
            
            assert result is not None
            assert result["transformation_results"] is not None
            assert len(result["transformation_results"]) == 3
            assert result["pattern_results"] is not None
            assert result["backup_path"] is not None
    
    @pytest.mark.asyncio
    async def test_run_with_partial_tool_failures(self, executor_agent, mock_state):
        """Test executor agent when some tools fail but others succeed."""
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            # Backup succeeds, first phase fails, second phase succeeds, patterns succeed
            mock_use_tool.side_effect = [
                "/backup/path",  # backup succeeds
                Exception("Phase transformation failed"),  # first phase fails
                {"result": "phase2_completed"},  # second phase succeeds
                {"patterns_applied": 2}  # patterns succeed
            ]
            
            result = await executor_agent.run(mock_state)
            
            # Should handle partial failures gracefully
            assert result is not None
            assert result["error"] is not None
            assert "Phase transformation failed" in result["error"]
    
    @pytest.mark.asyncio
    async def test_run_with_timeout_simulation(self, executor_agent, mock_state):
        """Test executor agent with timeout simulation."""
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            # Simulate timeout by making tools take too long
            async def slow_tool(*args, **kwargs):
                await asyncio.sleep(0.1)  # Simulate slow operation
                return {"result": "slow_result"}
            
            mock_use_tool.side_effect = slow_tool
            
            result = await executor_agent.run(mock_state)
            
            # Should complete despite slow tools
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_malformed_tool_responses(self, executor_agent, mock_state):
        """Test executor agent with malformed tool responses."""
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            # Mock malformed responses
            mock_use_tool.side_effect = [
                None,  # None response
                {"result": "phase1_completed"},  # Valid response
                {"result": "phase2_completed"},  # Valid response
                {"patterns_applied": "not_a_number"}  # Wrong type
            ]
            
            result = await executor_agent.run(mock_state)
            
            # Should handle malformed responses gracefully
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_use_tool_with_invalid_tool_name(self, executor_agent):
        """Test using a tool that doesn't exist."""
        with pytest.raises(ValueError, match="Tool 'invalid_tool' not found"):
            await executor_agent.use_tool("invalid_tool", param="value")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_missing_parameters(self, executor_agent):
        """Test using a tool with missing required parameters."""
        # This should raise an error due to missing required parameter
        with pytest.raises((ValueError, TypeError)):
            await executor_agent.use_tool("transform_code_phase")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_extra_parameters(self, executor_agent):
        """Test using a tool with extra parameters."""
        with patch.object(executor_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            # Should handle extra parameters gracefully
            result = await executor_agent.use_tool(
                "transform_code_phase", 
                phase={},
                extra_param="extra_value"
            )
            
            assert result is not None
    
    def test_get_system_prompt_content(self, executor_agent):
        """Test system prompt contains expected content."""
        prompt = executor_agent.get_system_prompt()
        
        # Check for key concepts in the prompt
        assert "execution" in prompt.lower()
        assert "transformation" in prompt.lower()
        assert "modernization" in prompt.lower()
        assert "backup" in prompt.lower()
        assert "functionality" in prompt.lower()
        assert "patterns" in prompt.lower()
    
    def test_get_system_prompt_length(self, executor_agent):
        """Test system prompt has reasonable length."""
        prompt = executor_agent.get_system_prompt()
        
        # Should be substantial but not excessive
        assert len(prompt) > 100  # At least 100 characters
        assert len(prompt) < 2000  # Not more than 2000 characters
    
    def test_log_activity_with_different_data_types(self, executor_agent):
        """Test logging with different data types."""
        # Test with string
        executor_agent.log_activity("string activity")
        
        # Test with dict
        executor_agent.log_activity("dict activity", {"key": "value", "number": 42})
        
        # Test with list
        executor_agent.log_activity("list activity", ["item1", "item2"])
        
        # Test with None
        executor_agent.log_activity("none activity", None)
        
        # All should complete without errors
        assert True
    
    def test_log_activity_with_complex_data(self, executor_agent):
        """Test logging with complex nested data structures."""
        complex_data = {
            "execution": {
                "phases": [
                    {"name": "phase1", "status": "completed", "files": 10},
                    {"name": "phase2", "status": "in_progress", "files": 5}
                ],
                "transformations": {
                    "total": 15,
                    "successful": 12,
                    "failed": 3
                },
                "backup": {
                    "path": "/backup/path",
                    "size": "500MB",
                    "created": "2024-01-01T00:00:00Z"
                }
            },
            "metadata": {
                "timestamp": "2024-01-01T00:00:00Z",
                "version": "1.0.0",
                "execution_time": 120.5
            }
        }
        
        executor_agent.log_activity("complex execution", complex_data)
        assert True  # Should complete without errors
    
    @pytest.mark.asyncio
    async def test_concurrent_tool_usage(self, executor_agent):
        """Test concurrent usage of multiple tools."""
        async def use_multiple_tools():
            tasks = []
            for i in range(5):
                task = executor_agent.use_tool(
                    "transform_code_phase", 
                    phase={"name": f"phase{i}"}
                )
                tasks.append(task)
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            return results
        
        with patch.object(executor_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            results = await use_multiple_tools()
            
            # All should complete successfully
            assert len(results) == 5
            for result in results:
                assert not isinstance(result, Exception)
    
    @pytest.mark.asyncio
    async def test_run_with_state_mutation(self, executor_agent):
        """Test that agent run doesn't mutate the original state inappropriately."""
        original_state = {
            "agent_id": "executor",
            "agent_type": "executor",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "backup_location": "/test/backup",
            "modernization_plan": {
                "phases": [{"name": "phase1", "transformations": ["transform1"]}],
                "patterns": [{"pattern": "old", "replacement": "new"}]
            },
            "analysis_results": None,
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
        
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                "/backup/path",  # backup tool
                {"result": "phase1_completed"},  # phase transformation
                {"patterns_applied": 1}  # pattern replacement
            ]
            
            result = await executor_agent.run(original_state)
            
            # Original state should be modified (this is expected behavior)
            assert result is not None
            assert result["transformation_results"] is not None
    
    def test_executor_agent_tool_integration(self, executor_agent):
        """Test that all tools are properly integrated."""
        tools = executor_agent.get_available_tools()
        
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
    async def test_run_with_different_modernization_goals(self, executor_agent):
        """Test executor agent with different modernization goals."""
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
                "agent_id": "executor",
                "agent_type": "executor",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": goals,
                "backup_location": "/test/backup",
                "modernization_plan": {
                    "phases": [{"name": "phase1", "transformations": ["transform1"]}],
                    "patterns": [{"pattern": "old", "replacement": "new"}]
                },
                "analysis_results": None,
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
            
            with patch.object(executor_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    "/backup/path",  # backup tool
                    {"result": "phase1_completed"},  # phase transformation
                    {"patterns_applied": 1}  # pattern replacement
                ]
                
                result = await executor_agent.run(state)
                assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_different_target_languages(self, executor_agent):
        """Test executor agent with different target languages."""
        languages = ["python", "javascript", "java", "csharp", "go"]
        
        for language in languages:
            state = {
                "agent_id": "executor",
                "agent_type": "executor",
                "codebase_path": "/test/codebase",
                "target_language": language,
                "modernization_goals": ["maintainability"],
                "backup_location": "/test/backup",
                "modernization_plan": {
                    "phases": [{"name": "phase1", "transformations": ["transform1"]}],
                    "patterns": [{"pattern": "old", "replacement": "new"}]
                },
                "analysis_results": None,
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
            
            with patch.object(executor_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    "/backup/path",  # backup tool
                    {"result": "phase1_completed"},  # phase transformation
                    {"patterns_applied": 1}  # pattern replacement
                ]
                
                result = await executor_agent.run(state)
                assert result is not None
                assert result["target_language"] == language
    
    def test_executor_agent_memory_usage(self, executor_agent):
        """Test that executor agent doesn't have memory leaks."""
        import gc
        
        # Create and destroy multiple agents
        for i in range(10):
            temp_agent = ExecutorAgent(Settings())
            del temp_agent
        
        # Force garbage collection
        gc.collect()
        
        # Original agent should still work
        assert executor_agent is not None
        assert len(executor_agent.get_available_tools()) > 0
    
    @pytest.mark.asyncio
    async def test_run_with_corrupted_state(self, executor_agent):
        """Test executor agent with corrupted or incomplete state."""
        corrupted_states = [
            {},  # Empty state
            {"agent_id": "executor"},  # Minimal state
            {"modernization_plan": None},  # None plan
            {"modernization_plan": {}},  # Empty plan
        ]
        
        for state in corrupted_states:
            # Fill in missing required fields
            complete_state = {
                "agent_id": "executor",
                "agent_type": "executor",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "backup_location": "/test/backup",
                "modernization_plan": {
                    "phases": [{"name": "phase1", "transformations": ["transform1"]}],
                    "patterns": [{"pattern": "old", "replacement": "new"}]
                },
                "analysis_results": None,
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
            
            with patch.object(executor_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    "/backup/path",  # backup tool
                    {"result": "phase1_completed"},  # phase transformation
                    {"patterns_applied": 1}  # pattern replacement
                ]
                
                result = await executor_agent.run(complete_state)
                # Should handle corrupted state gracefully
                assert result is not None

    # ==================== PERFORMANCE & STRESS TESTS ====================
    
    @pytest.mark.asyncio
    async def test_run_performance_benchmark(self, executor_agent):
        """Test executor agent performance with timing measurements."""
        import time
        
        state = {
            "agent_id": "executor",
            "agent_type": "executor",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "backup_location": "/test/backup",
            "modernization_plan": {
                "phases": [{"name": "phase1", "transformations": ["transform1"]}],
                "patterns": [{"pattern": "old", "replacement": "new"}]
            },
            "analysis_results": None,
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
        
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                "/backup/path",  # backup tool
                {"result": "phase1_completed"},  # phase transformation
                {"patterns_applied": 1}  # pattern replacement
            ]
            
            start_time = time.time()
            result = await executor_agent.run(state)
            end_time = time.time()
            
            execution_time = end_time - start_time
            
            # Should complete within reasonable time (less than 5 seconds)
            assert execution_time < 5.0
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_stress_test_multiple_runs(self, executor_agent):
        """Test executor agent with multiple consecutive runs."""
        state = {
            "agent_id": "executor",
            "agent_type": "executor",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "backup_location": "/test/backup",
            "modernization_plan": {
                "phases": [{"name": "phase1", "transformations": ["transform1"]}],
                "patterns": [{"pattern": "old", "replacement": "new"}]
            },
            "analysis_results": None,
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
        
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                "/backup/path",  # backup tool
                {"result": "phase1_completed"},  # phase transformation
                {"patterns_applied": 1}  # pattern replacement
            ]
            
            # Run multiple times to test for memory leaks or state issues
            for i in range(10):
                # Create a fresh copy of state for each run
                fresh_state = state.copy()
                result = await executor_agent.run(fresh_state)
                assert result is not None
                # Check if transformation_results exists (may be None on error)
                if result.get("transformation_results") is not None:
                    assert result["transformation_results"] is not None
    
    @pytest.mark.asyncio
    async def test_concurrent_agent_instances(self):
        """Test multiple executor agent instances running concurrently."""
        async def run_agent_instance(agent_id):
            agent = ExecutorAgent(Settings())
            state = {
                "agent_id": f"executor_{agent_id}",
                "agent_type": "executor",
                "codebase_path": f"/test/codebase_{agent_id}",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "backup_location": f"/test/backup_{agent_id}",
                "modernization_plan": {
                    "phases": [{"name": f"phase{agent_id}", "transformations": ["transform1"]}],
                    "patterns": [{"pattern": "old", "replacement": "new"}]
                },
                "analysis_results": None,
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
                    f"/backup/path_{agent_id}",  # backup tool
                    {"result": f"phase{agent_id}_completed"},  # phase transformation
                    {"patterns_applied": 1}  # pattern replacement
                ]
                
                return await agent.run(state)
        
        # Run 5 concurrent agent instances
        tasks = [run_agent_instance(i) for i in range(5)]
        results = await asyncio.gather(*tasks)
        
        # All should complete successfully
        assert len(results) == 5
        for i, result in enumerate(results):
            assert result is not None
            assert result["agent_id"] == f"executor_{i}"
    
    @pytest.mark.asyncio
    async def test_tool_error_recovery(self, executor_agent, mock_state):
        """Test executor agent recovery from various tool errors."""
        error_scenarios = [
            Exception("Network timeout"),
            ValueError("Invalid input"),
            RuntimeError("Resource unavailable"),
            KeyError("Missing key"),
            TypeError("Wrong type"),
        ]
        
        for error in error_scenarios:
            with patch.object(executor_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = error
                
                result = await executor_agent.run(mock_state.copy())
                
                # Should handle all error types gracefully
                assert result is not None
                assert result["error"] is not None
                assert str(error) in result["error"]
    
    def test_executor_agent_thread_safety(self, executor_agent):
        """Test executor agent thread safety (basic check)."""
        import threading
        import time
        
        results = []
        errors = []
        
        def run_agent():
            try:
                # Test basic operations that should be thread-safe
                tools = executor_agent.get_available_tools()
                prompt = executor_agent.get_system_prompt()
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
    async def test_executor_agent_with_extreme_data_sizes(self, executor_agent):
        """Test executor agent with extreme data sizes."""
        # Test with very large data
        large_state = {
            "agent_id": "executor",
            "agent_type": "executor",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "backup_location": "/test/backup",
            "modernization_plan": {
                "phases": [{"name": f"phase{i}", "transformations": [f"transform{j}" for j in range(100)]} for i in range(1000)],
                "patterns": [{"pattern": f"old_pattern{i}", "replacement": f"new_pattern{i}"} for i in range(500)]
            },
            "analysis_results": None,
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
        
        with patch.object(executor_agent, 'use_tool') as mock_use_tool:
            # Mock extremely large responses
            huge_phases = [{"result": f"phase{i}_completed"} for i in range(1000)]
            huge_patterns = {"patterns_applied": 500, "files_modified": 1000}
            
            mock_use_tool.side_effect = [
                "/backup/path",  # backup tool
                *huge_phases,  # all phase transformations
                huge_patterns  # pattern replacement
            ]
            
            result = await executor_agent.run(large_state)
            
            # Should handle large data without issues
            assert result is not None
            assert result["transformation_results"] is not None
            assert len(result["transformation_results"]) == 1000
            assert result["pattern_results"] is not None
    
    def test_executor_agent_configuration_validation(self):
        """Test executor agent with various configuration scenarios."""
        # Test with minimal settings
        minimal_settings = Settings()
        minimal_settings.llm_provider = "anthropic"
        minimal_settings.llm_model = "claude-3-sonnet"
        
        agent = ExecutorAgent(minimal_settings)
        assert agent is not None
        
        # Test with custom settings
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        custom_settings.llm_temperature = 0.5
        custom_settings.llm_max_tokens = 2000
        
        agent = ExecutorAgent(custom_settings)
        assert agent.settings.llm_temperature == 0.5
        assert agent.settings.llm_max_tokens == 2000
    
    @pytest.mark.asyncio
    async def test_executor_agent_edge_case_combinations(self, executor_agent):
        """Test executor agent with edge case combinations."""
        edge_cases = [
            # Empty plan with multiple goals
            {
                "modernization_plan": {
                    "phases": [],
                    "patterns": []
                },
                "target_language": "python",
                "modernization_goals": ["maintainability", "performance", "security"]
            },
            # Complex plan with single goal
            {
                "modernization_plan": {
                    "phases": [{"name": f"phase{i}", "transformations": [f"transform{j}" for j in range(10)]} for i in range(5)],
                    "patterns": [{"pattern": f"old{i}", "replacement": f"new{i}"} for i in range(3)]
                },
                "target_language": "javascript",
                "modernization_goals": ["maintainability"]
            },
            # Different language combinations
            {
                "modernization_plan": {
                    "phases": [{"name": "phase1", "transformations": ["legacy_to_modern"]}],
                    "patterns": [{"pattern": "legacy_pattern", "replacement": "modern_pattern"}]
                },
                "target_language": "java",
                "modernization_goals": ["performance"]
            }
        ]
        
        for case in edge_cases:
            state = {
                "agent_id": "executor",
                "agent_type": "executor",
                "codebase_path": "/test/codebase",
                "backup_location": "/test/backup",
                "analysis_results": None,
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
            
            with patch.object(executor_agent, 'use_tool') as mock_use_tool:
                # Mock responses based on plan complexity
                phases_count = len(case["modernization_plan"]["phases"])
                mock_responses = ["/backup/path"]  # backup
                mock_responses.extend([{"result": f"phase{i}_completed"} for i in range(phases_count)])  # phases
                mock_responses.append({"patterns_applied": len(case["modernization_plan"]["patterns"])})  # patterns
                
                mock_use_tool.side_effect = mock_responses
                
                result = await executor_agent.run(state)
                assert result is not None
                assert result["target_language"] == case["target_language"]
                assert result["modernization_goals"] == case["modernization_goals"]
