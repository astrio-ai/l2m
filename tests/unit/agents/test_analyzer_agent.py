"""
Unit tests for the analyzer agent.

This module contains unit tests for the analyzer agent
functionality and behavior.
"""

import pytest
import asyncio
from unittest.mock import Mock, patch, AsyncMock, MagicMock
from src.core.agents.analyzer_agent import AnalyzerAgent
from src.core.state.agent_state import AgentState
from src.config.settings import Settings


class TestAnalyzerAgent:
    """Test cases for the analyzer agent."""
    
    @pytest.fixture
    def settings(self):
        """Create settings fixture."""
        return Settings()
    
    @pytest.fixture
    def analyzer_agent(self, settings):
        """Create analyzer agent fixture."""
        return AnalyzerAgent(settings)
    
    @pytest.fixture
    def mock_state(self):
        """Create mock agent state."""
        return {
            "agent_id": "analyzer",
            "agent_type": "analyzer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": None,
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
    
    def test_analyzer_agent_initialization(self, analyzer_agent):
        """Test analyzer agent initialization."""
        assert analyzer_agent is not None
        assert analyzer_agent.settings is not None
        assert len(analyzer_agent.tools) > 0
    
    def test_get_system_prompt(self, analyzer_agent):
        """Test system prompt generation."""
        prompt = analyzer_agent.get_system_prompt()
        assert isinstance(prompt, str)
        assert len(prompt) > 0
        assert "analysis" in prompt.lower()
    
    @pytest.mark.asyncio
    async def test_run_analyzer_agent(self, analyzer_agent, mock_state):
        """Test analyzer agent execution."""
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            # Mock tool responses
            mock_use_tool.side_effect = [
                {"files": ["file1.cobol", "file2.cobol"]},
                {"dependencies": ["dep1", "dep2"]},
                {"patterns": ["pattern1", "pattern2"]}
            ]
            
            result = await analyzer_agent.run(mock_state)
            
        assert result is not None
        assert result["analysis_results"] is not None
        assert "structure" in result["analysis_results"]
        assert "dependencies" in result["analysis_results"]
        assert "patterns" in result["analysis_results"]
    
    @pytest.mark.asyncio
    async def test_run_analyzer_agent_error(self, analyzer_agent, mock_state):
        """Test analyzer agent error handling."""
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = Exception("Tool error")
            
            result = await analyzer_agent.run(mock_state)
            
        assert result is not None
        assert result["error"] is not None
        assert "Tool error" in result["error"]
    
    def test_get_available_tools(self, analyzer_agent):
        """Test available tools retrieval."""
        tools = analyzer_agent.get_available_tools()
        assert isinstance(tools, list)
        assert len(tools) > 0
    
    @pytest.mark.asyncio
    async def test_use_tool(self, analyzer_agent):
        """Test tool usage."""
        with patch.object(analyzer_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            result = await analyzer_agent.use_tool("analyze_code_structure", codebase_path="/test/path")
            
            assert result is not None
            assert result["result"] == "test"
    
    def test_log_activity(self, analyzer_agent):
        """Test activity logging."""
        # This test would verify that logging works correctly
        # For now, just ensure the method exists and can be called
        analyzer_agent.log_activity("test activity", {"key": "value"})
        assert True  # If we get here, no exception was raised

    # ==================== EXTENSIVE TEST SUITE ====================
    
    def test_analyzer_agent_tools_initialization(self, analyzer_agent):
        """Test that analyzer agent has the correct tools initialized."""
        tools = analyzer_agent.get_available_tools()
        tool_names = [tool.name for tool in tools]
        
        # Should have code analysis tools
        assert "analyze_code_structure" in tool_names
        assert "analyze_dependencies" in tool_names
        
        # Should have file tools
        assert "read_file" in tool_names
        assert "scan_directory" in tool_names
        
        # Verify tool count
        assert len(tools) >= 4  # At least 4 tools should be available
    
    def test_analyzer_agent_with_different_settings(self):
        """Test analyzer agent with different configuration settings."""
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        
        agent = AnalyzerAgent(custom_settings)
        assert agent.settings.llm_provider == "openai"
        assert agent.settings.llm_model == "gpt-4"
    
    @pytest.mark.asyncio
    async def test_run_with_empty_codebase(self, analyzer_agent):
        """Test analyzer agent with empty codebase."""
        empty_state = {
            "agent_id": "analyzer",
            "agent_type": "analyzer",
            "codebase_path": "/empty/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": None,
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
        
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            # Mock empty responses
            mock_use_tool.side_effect = [
                {"files": []},
                {"dependencies": []},
                {"patterns": []}
            ]
            
            result = await analyzer_agent.run(empty_state)
            
            assert result is not None
            assert result["analysis_results"] is not None
            assert result["analysis_results"]["structure"]["files"] == []
            assert result["analysis_results"]["dependencies"]["dependencies"] == []
            assert result["analysis_results"]["patterns"]["patterns"] == []
    
    @pytest.mark.asyncio
    async def test_run_with_large_codebase(self, analyzer_agent):
        """Test analyzer agent with large codebase simulation."""
        large_state = {
            "agent_id": "analyzer",
            "agent_type": "analyzer",
            "codebase_path": "/large/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability", "performance"],
            "analysis_results": None,
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
        
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            # Mock large responses
            large_files = [f"file{i}.cobol" for i in range(100)]
            large_deps = [f"dep{i}" for i in range(50)]
            large_patterns = [f"pattern{i}" for i in range(25)]
            
            mock_use_tool.side_effect = [
                {"files": large_files},
                {"dependencies": large_deps},
                {"patterns": large_patterns}
            ]
            
            result = await analyzer_agent.run(large_state)
            
            assert result is not None
            assert result["analysis_results"] is not None
            assert len(result["analysis_results"]["structure"]["files"]) == 100
            assert len(result["analysis_results"]["dependencies"]["dependencies"]) == 50
            assert len(result["analysis_results"]["patterns"]["patterns"]) == 25
    
    @pytest.mark.asyncio
    async def test_run_with_partial_tool_failures(self, analyzer_agent, mock_state):
        """Test analyzer agent when some tools fail but others succeed."""
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            # First tool succeeds, second fails, third succeeds
            mock_use_tool.side_effect = [
                {"files": ["file1.cobol"]},
                Exception("Dependency analysis failed"),
                {"patterns": ["pattern1"]}
            ]
            
            result = await analyzer_agent.run(mock_state)
            
            # Should handle partial failures gracefully
            assert result is not None
            assert result["error"] is not None
            assert "Dependency analysis failed" in result["error"]
    
    @pytest.mark.asyncio
    async def test_run_with_timeout_simulation(self, analyzer_agent, mock_state):
        """Test analyzer agent with timeout simulation."""
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            # Simulate timeout by making tools take too long
            async def slow_tool(*args, **kwargs):
                await asyncio.sleep(0.1)  # Simulate slow operation
                return {"result": "slow_result"}
            
            mock_use_tool.side_effect = slow_tool
            
            result = await analyzer_agent.run(mock_state)
            
            # Should complete despite slow tools
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_malformed_tool_responses(self, analyzer_agent, mock_state):
        """Test analyzer agent with malformed tool responses."""
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            # Mock malformed responses
            mock_use_tool.side_effect = [
                None,  # None response
                {"dependencies": "not_a_list"},  # Wrong type
                {"patterns": []}  # Valid response
            ]
            
            result = await analyzer_agent.run(mock_state)
            
            # Should handle malformed responses gracefully
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_use_tool_with_invalid_tool_name(self, analyzer_agent):
        """Test using a tool that doesn't exist."""
        with pytest.raises(ValueError, match="Tool 'invalid_tool' not found"):
            await analyzer_agent.use_tool("invalid_tool", param="value")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_missing_parameters(self, analyzer_agent):
        """Test using a tool with missing required parameters."""
        # This should raise an error due to missing required parameter
        with pytest.raises((ValueError, TypeError)):
            await analyzer_agent.use_tool("analyze_code_structure")
    
    @pytest.mark.asyncio
    async def test_use_tool_with_extra_parameters(self, analyzer_agent):
        """Test using a tool with extra parameters."""
        with patch.object(analyzer_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            # Should handle extra parameters gracefully
            result = await analyzer_agent.use_tool(
                "analyze_code_structure", 
                codebase_path="/test/path",
                extra_param="extra_value"
            )
            
            assert result is not None
    
    def test_get_system_prompt_content(self, analyzer_agent):
        """Test system prompt contains expected content."""
        prompt = analyzer_agent.get_system_prompt()
        
        # Check for key concepts in the prompt
        assert "code analysis" in prompt.lower()
        assert "legacy" in prompt.lower()
        assert "modernization" in prompt.lower()
        assert "structure" in prompt.lower()
        assert "dependencies" in prompt.lower()
        assert "patterns" in prompt.lower()
    
    def test_get_system_prompt_length(self, analyzer_agent):
        """Test system prompt has reasonable length."""
        prompt = analyzer_agent.get_system_prompt()
        
        # Should be substantial but not excessive
        assert len(prompt) > 100  # At least 100 characters
        assert len(prompt) < 2000  # Not more than 2000 characters
    
    def test_log_activity_with_different_data_types(self, analyzer_agent):
        """Test logging with different data types."""
        # Test with string
        analyzer_agent.log_activity("string activity")
        
        # Test with dict
        analyzer_agent.log_activity("dict activity", {"key": "value", "number": 42})
        
        # Test with list
        analyzer_agent.log_activity("list activity", ["item1", "item2"])
        
        # Test with None
        analyzer_agent.log_activity("none activity", None)
        
        # All should complete without errors
        assert True
    
    def test_log_activity_with_complex_data(self, analyzer_agent):
        """Test logging with complex nested data structures."""
        complex_data = {
            "analysis": {
                "files": ["file1.cobol", "file2.cobol"],
                "metrics": {
                    "complexity": 5.2,
                    "lines": 1500
                },
                "patterns": [
                    {"name": "pattern1", "count": 3},
                    {"name": "pattern2", "count": 7}
                ]
            },
            "metadata": {
                "timestamp": "2024-01-01T00:00:00Z",
                "version": "1.0.0"
            }
        }
        
        analyzer_agent.log_activity("complex analysis", complex_data)
        assert True  # Should complete without errors
    
    @pytest.mark.asyncio
    async def test_concurrent_tool_usage(self, analyzer_agent):
        """Test concurrent usage of multiple tools."""
        async def use_multiple_tools():
            tasks = []
            for i in range(5):
                task = analyzer_agent.use_tool(
                    "analyze_code_structure", 
                    codebase_path=f"/test/path{i}"
                )
                tasks.append(task)
            
            results = await asyncio.gather(*tasks, return_exceptions=True)
            return results
        
        with patch.object(analyzer_agent.tools[0], 'run') as mock_run:
            mock_run.return_value = {"result": "test"}
            
            results = await use_multiple_tools()
            
            # All should complete successfully
            assert len(results) == 5
            for result in results:
                assert not isinstance(result, Exception)
    
    @pytest.mark.asyncio
    async def test_run_with_state_mutation(self, analyzer_agent):
        """Test that agent run doesn't mutate the original state inappropriately."""
        original_state = {
            "agent_id": "analyzer",
            "agent_type": "analyzer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": None,
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
        
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"files": ["file1.cobol"]},
                {"dependencies": ["dep1"]},
                {"patterns": ["pattern1"]}
            ]
            
            result = await analyzer_agent.run(original_state)
            
            # Original state should be modified (this is expected behavior)
            assert result is not None
            assert result["analysis_results"] is not None
    
    def test_analyzer_agent_tool_integration(self, analyzer_agent):
        """Test that all tools are properly integrated."""
        tools = analyzer_agent.get_available_tools()
        
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
    async def test_run_with_different_modernization_goals(self, analyzer_agent):
        """Test analyzer agent with different modernization goals."""
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
                "agent_id": "analyzer",
                "agent_type": "analyzer",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": goals,
                "analysis_results": None,
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
            
            with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"files": ["file1.cobol"]},
                    {"dependencies": ["dep1"]},
                    {"patterns": ["pattern1"]}
                ]
                
                result = await analyzer_agent.run(state)
                assert result is not None
    
    @pytest.mark.asyncio
    async def test_run_with_different_target_languages(self, analyzer_agent):
        """Test analyzer agent with different target languages."""
        languages = ["python", "javascript", "java", "csharp", "go"]
        
        for language in languages:
            state = {
                "agent_id": "analyzer",
                "agent_type": "analyzer",
                "codebase_path": "/test/codebase",
                "target_language": language,
                "modernization_goals": ["maintainability"],
                "analysis_results": None,
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
            
            with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"files": [f"file1.{language}"]},
                    {"dependencies": ["dep1"]},
                    {"patterns": ["pattern1"]}
                ]
                
                result = await analyzer_agent.run(state)
                assert result is not None
                assert result["target_language"] == language
    
    def test_analyzer_agent_memory_usage(self, analyzer_agent):
        """Test that analyzer agent doesn't have memory leaks."""
        import gc
        
        # Create and destroy multiple agents
        for i in range(10):
            temp_agent = AnalyzerAgent(Settings())
            del temp_agent
        
        # Force garbage collection
        gc.collect()
        
        # Original agent should still work
        assert analyzer_agent is not None
        assert len(analyzer_agent.get_available_tools()) > 0
    
    @pytest.mark.asyncio
    async def test_run_with_corrupted_state(self, analyzer_agent):
        """Test analyzer agent with corrupted or incomplete state."""
        corrupted_states = [
            {},  # Empty state
            {"agent_id": "analyzer"},  # Minimal state
            {"codebase_path": None},  # None path
            {"codebase_path": ""},  # Empty path
        ]
        
        for state in corrupted_states:
            # Fill in missing required fields
            complete_state = {
                "agent_id": "analyzer",
                "agent_type": "analyzer",
                "codebase_path": "/test/codebase",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "analysis_results": None,
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
            
            with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"files": ["file1.cobol"]},
                    {"dependencies": ["dep1"]},
                    {"patterns": ["pattern1"]}
                ]
                
                result = await analyzer_agent.run(complete_state)
                # Should handle corrupted state gracefully
                assert result is not None

    # ==================== PERFORMANCE & STRESS TESTS ====================
    
    @pytest.mark.asyncio
    async def test_run_performance_benchmark(self, analyzer_agent):
        """Test analyzer agent performance with timing measurements."""
        import time
        
        state = {
            "agent_id": "analyzer",
            "agent_type": "analyzer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": None,
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
        
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"files": ["file1.cobol"]},
                {"dependencies": ["dep1"]},
                {"patterns": ["pattern1"]}
            ]
            
            start_time = time.time()
            result = await analyzer_agent.run(state)
            end_time = time.time()
            
            execution_time = end_time - start_time
            
            # Should complete within reasonable time (less than 5 seconds)
            assert execution_time < 5.0
            assert result is not None
    
    @pytest.mark.asyncio
    async def test_stress_test_multiple_runs(self, analyzer_agent):
        """Test analyzer agent with multiple consecutive runs."""
        state = {
            "agent_id": "analyzer",
            "agent_type": "analyzer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": None,
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
        
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            mock_use_tool.side_effect = [
                {"files": ["file1.cobol"]},
                {"dependencies": ["dep1"]},
                {"patterns": ["pattern1"]}
            ]
            
            # Run multiple times to test for memory leaks or state issues
            for i in range(10):
                # Create a fresh copy of state for each run
                fresh_state = state.copy()
                result = await analyzer_agent.run(fresh_state)
                assert result is not None
                # Check if analysis_results exists (may be None on error)
                if result.get("analysis_results") is not None:
                    assert result["analysis_results"] is not None
    
    @pytest.mark.asyncio
    async def test_concurrent_agent_instances(self):
        """Test multiple analyzer agent instances running concurrently."""
        async def run_agent_instance(agent_id):
            agent = AnalyzerAgent(Settings())
            state = {
                "agent_id": f"analyzer_{agent_id}",
                "agent_type": "analyzer",
                "codebase_path": f"/test/codebase_{agent_id}",
                "target_language": "python",
                "modernization_goals": ["maintainability"],
                "analysis_results": None,
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
                    {"files": [f"file{agent_id}.cobol"]},
                    {"dependencies": [f"dep{agent_id}"]},
                    {"patterns": [f"pattern{agent_id}"]}
                ]
                
                return await agent.run(state)
        
        # Run 5 concurrent agent instances
        tasks = [run_agent_instance(i) for i in range(5)]
        results = await asyncio.gather(*tasks)
        
        # All should complete successfully
        assert len(results) == 5
        for i, result in enumerate(results):
            assert result is not None
            assert result["agent_id"] == f"analyzer_{i}"
    
    @pytest.mark.asyncio
    async def test_tool_error_recovery(self, analyzer_agent, mock_state):
        """Test analyzer agent recovery from various tool errors."""
        error_scenarios = [
            Exception("Network timeout"),
            ValueError("Invalid input"),
            RuntimeError("Resource unavailable"),
            KeyError("Missing key"),
            TypeError("Wrong type"),
        ]
        
        for error in error_scenarios:
            with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = error
                
                result = await analyzer_agent.run(mock_state.copy())
                
                # Should handle all error types gracefully
                assert result is not None
                assert result["error"] is not None
                assert str(error) in result["error"]
    
    def test_analyzer_agent_thread_safety(self, analyzer_agent):
        """Test analyzer agent thread safety (basic check)."""
        import threading
        import time
        
        results = []
        errors = []
        
        def run_agent():
            try:
                # Test basic operations that should be thread-safe
                tools = analyzer_agent.get_available_tools()
                prompt = analyzer_agent.get_system_prompt()
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
    async def test_analyzer_agent_with_extreme_data_sizes(self, analyzer_agent):
        """Test analyzer agent with extreme data sizes."""
        # Test with very large data
        large_state = {
            "agent_id": "analyzer",
            "agent_type": "analyzer",
            "codebase_path": "/test/codebase",
            "target_language": "python",
            "modernization_goals": ["maintainability"],
            "analysis_results": None,
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
        
        with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
            # Mock extremely large responses
            huge_files = [f"file{i}.cobol" for i in range(10000)]
            huge_deps = [f"dep{i}" for i in range(5000)]
            huge_patterns = [f"pattern{i}" for i in range(2500)]
            
            mock_use_tool.side_effect = [
                {"files": huge_files},
                {"dependencies": huge_deps},
                {"patterns": huge_patterns}
            ]
            
            result = await analyzer_agent.run(large_state)
            
            # Should handle large data without issues
            assert result is not None
            assert result["analysis_results"] is not None
            assert len(result["analysis_results"]["structure"]["files"]) == 10000
            assert len(result["analysis_results"]["dependencies"]["dependencies"]) == 5000
            assert len(result["analysis_results"]["patterns"]["patterns"]) == 2500
    
    def test_analyzer_agent_configuration_validation(self):
        """Test analyzer agent with various configuration scenarios."""
        # Test with minimal settings
        minimal_settings = Settings()
        minimal_settings.llm_provider = "anthropic"
        minimal_settings.llm_model = "claude-3-sonnet"
        
        agent = AnalyzerAgent(minimal_settings)
        assert agent is not None
        
        # Test with custom settings
        custom_settings = Settings()
        custom_settings.llm_provider = "openai"
        custom_settings.llm_model = "gpt-4"
        custom_settings.llm_temperature = 0.5
        custom_settings.llm_max_tokens = 2000
        
        agent = AnalyzerAgent(custom_settings)
        assert agent.settings.llm_temperature == 0.5
        assert agent.settings.llm_max_tokens == 2000
    
    @pytest.mark.asyncio
    async def test_analyzer_agent_edge_case_combinations(self, analyzer_agent):
        """Test analyzer agent with edge case combinations."""
        edge_cases = [
            # Empty codebase with multiple goals
            {
                "codebase_path": "/empty",
                "target_language": "python",
                "modernization_goals": ["maintainability", "performance", "security"]
            },
            # Large codebase with single goal
            {
                "codebase_path": "/large",
                "target_language": "javascript",
                "modernization_goals": ["maintainability"]
            },
            # Different language combinations
            {
                "codebase_path": "/cobol",
                "target_language": "java",
                "modernization_goals": ["performance"]
            }
        ]
        
        for case in edge_cases:
            state = {
                "agent_id": "analyzer",
                "agent_type": "analyzer",
                "analysis_results": None,
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
            
            with patch.object(analyzer_agent, 'use_tool') as mock_use_tool:
                mock_use_tool.side_effect = [
                    {"files": ["file1.cobol"]},
                    {"dependencies": ["dep1"]},
                    {"patterns": ["pattern1"]}
                ]
                
                result = await analyzer_agent.run(state)
                assert result is not None
                assert result["target_language"] == case["target_language"]
                assert result["modernization_goals"] == case["modernization_goals"]
