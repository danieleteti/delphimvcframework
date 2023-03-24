/* system statistics extension */

-- complain if script is sourced in psql, rather than via CREATE EXTENSION
\echo Use "CREATE EXTENSION system_stats" to load this file. \quit

-- role to be assigned while executing functions of system stats
-- before creating role, check the role exists or not. It may possible
-- that user want to create extension in multiple database of same server
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'monitor_system_stats') THEN
        CREATE ROLE monitor_system_stats WITH
            NOLOGIN
            NOSUPERUSER
            NOCREATEDB
            NOCREATEROLE
            INHERIT
            NOREPLICATION
            CONNECTION LIMIT -1;
    END IF;
END
$$;

-- Operating system information function
CREATE FUNCTION pg_sys_os_info(
    OUT name text,
    OUT version text,
    OUT host_name text,
    OUT domain_name text,
    OUT handle_count int,
    OUT process_count int,
    OUT thread_count int,
    OUT architecture text,
    OUT last_bootup_time text,
    OUT os_up_since_seconds int
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C;

REVOKE ALL ON FUNCTION pg_sys_os_info() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION pg_sys_os_info() TO monitor_system_stats;

-- System CPU information function
CREATE FUNCTION pg_sys_cpu_info(            
    OUT vendor text,
    OUT description text,
    OUT model_name text,
    OUT processor_type int,
    OUT logical_processor int,
    OUT physical_processor int,
    OUT no_of_cores int,
    OUT architecture text,
    OUT clock_speed_hz int8,
    OUT cpu_type text,
    OUT cpu_family text,
    OUT byte_order text,
    OUT l1dcache_size int,
    OUT l1icache_size int,
    OUT l2cache_size int,
    OUT l3cache_size int
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C;

REVOKE ALL ON FUNCTION pg_sys_cpu_info() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION pg_sys_cpu_info() TO monitor_system_stats; 

-- Memory information function
CREATE FUNCTION pg_sys_memory_info(
    OUT total_memory int8,
    OUT used_memory int8,
    OUT free_memory int8,
    OUT swap_total int8,
    OUT swap_used int8,
    OUT swap_free int8,
    OUT cache_total int8,
    OUT kernel_total int8,
    OUT kernel_paged int8,
    OUT kernel_non_paged int8,
    OUT total_page_file int8,
    OUT avail_page_file int8            
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C;

REVOKE ALL ON FUNCTION pg_sys_memory_info() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION pg_sys_memory_info() TO monitor_system_stats;

-- Load average information function
CREATE FUNCTION pg_sys_load_avg_info(
    OUT load_avg_one_minute float4,
    OUT load_avg_five_minutes float4,
    OUT load_avg_ten_minutes float4,
    OUT load_avg_fifteen_minutes float4
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C;

REVOKE ALL ON FUNCTION pg_sys_load_avg_info() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION pg_sys_load_avg_info() TO monitor_system_stats;

-- network information function
CREATE FUNCTION pg_sys_network_info(
    OUT interface_name text,
    OUT ip_address text,
    OUT tx_bytes int8,
    OUT tx_packets int8,
    OUT tx_errors int8,
    OUT tx_dropped int8,
    OUT rx_bytes int8,
    OUT rx_packets int8,
    OUT rx_errors int8,
    OUT rx_dropped int8,
    OUT link_speed_mbps int
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C;

REVOKE ALL ON FUNCTION pg_sys_network_info() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION pg_sys_network_info() TO monitor_system_stats;

-- CPU and memory information by process id or name
CREATE FUNCTION pg_sys_cpu_memory_by_process(
    OUT pid int,
    OUT name text,
    OUT running_since_seconds int8,
    OUT cpu_usage float4,
    OUT memory_usage float4,
    OUT memory_bytes int8
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C;

REVOKE ALL ON FUNCTION pg_sys_cpu_memory_by_process() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION pg_sys_cpu_memory_by_process() TO monitor_system_stats;

-- Disk information function
CREATE FUNCTION pg_sys_disk_info(
    OUT mount_point text,
    OUT file_system text,
    OUT drive_letter text,
    OUT drive_type int,
    OUT file_system_type text,
    OUT total_space int8,
    OUT used_space int8,
    OUT free_space int8,
    OUT total_inodes int8,
    OUT used_inodes int8,
    OUT free_inodes int8
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C;

REVOKE ALL ON FUNCTION pg_sys_disk_info() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION pg_sys_disk_info() TO monitor_system_stats;

-- process information function
CREATE FUNCTION pg_sys_process_info(
    OUT total_processes int,
    OUT running_processes int,
    OUT sleeping_processes int,
    OUT stopped_processes int,
    OUT zombie_processes int
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C;

REVOKE ALL ON FUNCTION pg_sys_process_info() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION pg_sys_process_info() TO monitor_system_stats;

-- CPU usage information function
-- This function will fetch the time spent in percentage by CPU in each mode
-- as described by arguments
CREATE FUNCTION pg_sys_cpu_usage_info(
    OUT usermode_normal_process_percent float4,
    OUT usermode_niced_process_percent float4,
    OUT kernelmode_process_percent float4,
    OUT idle_mode_percent float4,
    OUT IO_completion_percent float4,
    OUT servicing_irq_percent float4,
    OUT servicing_softirq_percent float4,
    OUT user_time_percent float4,
    OUT processor_time_percent float4,
    OUT privileged_time_percent float4,
    OUT interrupt_time_percent float4
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C;

REVOKE ALL ON FUNCTION pg_sys_cpu_usage_info() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION pg_sys_cpu_usage_info() TO monitor_system_stats;

-- IO analysis information function
CREATE FUNCTION pg_sys_io_analysis_info(
	OUT device_name text,
	OUT total_reads int8,
	OUT total_writes int8,
	OUT read_bytes int8,
	OUT write_bytes int8,
	OUT read_time_ms int8,
	OUT write_time_ms int8
)
RETURNS SETOF record
AS 'MODULE_PATHNAME'
LANGUAGE C;

REVOKE ALL ON FUNCTION pg_sys_io_analysis_info() FROM PUBLIC;
GRANT EXECUTE ON FUNCTION pg_sys_io_analysis_info() TO monitor_system_stats;


